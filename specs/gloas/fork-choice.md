# Gloas -- Fork Choice

*Note*: This document is a work-in-progress for researchers and implementers.

<!-- mdformat-toc start --slug=github --no-anchors --maxlevel=6 --minlevel=2 -->

- [Introduction](#introduction)
- [Constants](#constants)
- [Helpers](#helpers)
  - [Modified `Store`](#modified-store)
  - [Modified `get_forkchoice_store`](#modified-get_forkchoice_store)
  - [New `notify_ptc_messages`](#new-notify_ptc_messages)
  - [New `is_payload_timely`](#new-is_payload_timely)
  - [New `is_payload_data_available`](#new-is_payload_data_available)
  - [New `can_extend_payload`](#new-can_extend_payload)
  - [New `is_within_ptc_advisory_window`](#new-is_within_ptc_advisory_window)
  - [New `should_extend_payload`](#new-should_extend_payload)
  - [New `is_ptc_disrespecting_block`](#new-is_ptc_disrespecting_block)
  - [New `should_apply_proposer_boost`](#new-should_apply_proposer_boost)
  - [Modified `get_weight`](#modified-get_weight)
  - [Modified `get_head`](#modified-get_head)
  - [Modified `record_block_timeliness`](#modified-record_block_timeliness)
  - [Modified `update_proposer_boost_root`](#modified-update_proposer_boost_root)
  - [Modified `is_head_late`](#modified-is_head_late)
  - [Modified `is_head_weak`](#modified-is_head_weak)
  - [Modified `get_attestation_due_ms`](#modified-get_attestation_due_ms)
  - [Modified `get_aggregate_due_ms`](#modified-get_aggregate_due_ms)
  - [Modified `get_sync_message_due_ms`](#modified-get_sync_message_due_ms)
  - [Modified `get_contribution_due_ms`](#modified-get_contribution_due_ms)
  - [New `get_payload_attestation_due_ms`](#new-get_payload_attestation_due_ms)
- [Handlers](#handlers)
  - [Modified `on_block`](#modified-on_block)
  - [Modified `is_data_available`](#modified-is_data_available)
  - [New `on_execution_payload`](#new-on_execution_payload)
  - [New `on_payload_attestation_message`](#new-on_payload_attestation_message)

<!-- mdformat-toc end -->

## Introduction

This is the modification of the fork-choice accompanying the Gloas upgrade.

## Constants

| Name                                 | Value                   |
| ------------------------------------ | ----------------------- |
| `PAYLOAD_TIMELY_THRESHOLD`           | `PTC_SIZE // 2` (= 256) |
| `DATA_AVAILABILITY_TIMELY_THRESHOLD` | `PTC_SIZE // 2` (= 256) |
| `ATTESTATION_TIMELINESS_INDEX`       | `0`                     |
| `PTC_TIMELINESS_INDEX`               | `1`                     |
| `NUM_BLOCK_TIMELINESS_DEADLINES`     | `2`                     |

## Helpers

### Modified `Store`

*Note*: `Store` is modified to track blocks whose execution payloads have been
verified.

```python
@dataclass
class Store(object):
    time: uint64
    genesis_time: uint64
    justified_checkpoint: Checkpoint
    finalized_checkpoint: Checkpoint
    unrealized_justified_checkpoint: Checkpoint
    unrealized_finalized_checkpoint: Checkpoint
    proposer_boost_root: Root
    equivocating_indices: Set[ValidatorIndex]
    blocks: Dict[Root, BeaconBlock] = field(default_factory=dict)
    block_states: Dict[Root, BeaconState] = field(default_factory=dict)
    block_timeliness: Dict[Root, Vector[boolean, NUM_BLOCK_TIMELINESS_DEADLINES]] = field(
        default_factory=dict
    )
    checkpoint_states: Dict[Checkpoint, BeaconState] = field(default_factory=dict)
    latest_messages: Dict[ValidatorIndex, LatestMessage] = field(default_factory=dict)
    unrealized_justifications: Dict[Root, Checkpoint] = field(default_factory=dict)
    # [New in Gloas:EIP7732]
    payloads: Set[Root] = field(default_factory=set)
    # [New in Gloas:EIP7732]
    payload_timeliness_vote: Dict[Root, Vector[boolean, PTC_SIZE]] = field(default_factory=dict)
    # [New in Gloas:EIP7732]
    payload_data_availability_vote: Dict[Root, Vector[boolean, PTC_SIZE]] = field(
        default_factory=dict
    )
```

### Modified `get_forkchoice_store`

```python
def get_forkchoice_store(anchor_state: BeaconState, anchor_block: BeaconBlock) -> Store:
    assert anchor_block.state_root == hash_tree_root(anchor_state)
    anchor_root = hash_tree_root(anchor_block)
    anchor_epoch = get_current_epoch(anchor_state)
    justified_checkpoint = Checkpoint(epoch=anchor_epoch, root=anchor_root)
    finalized_checkpoint = Checkpoint(epoch=anchor_epoch, root=anchor_root)
    proposer_boost_root = Root()
    return Store(
        time=uint64(anchor_state.genesis_time + SLOT_DURATION_MS * anchor_state.slot // 1000),
        genesis_time=anchor_state.genesis_time,
        justified_checkpoint=justified_checkpoint,
        finalized_checkpoint=finalized_checkpoint,
        unrealized_justified_checkpoint=justified_checkpoint,
        unrealized_finalized_checkpoint=finalized_checkpoint,
        proposer_boost_root=proposer_boost_root,
        equivocating_indices=set(),
        blocks={anchor_root: copy(anchor_block)},
        block_states={anchor_root: copy(anchor_state)},
        # [New in Gloas:EIP7732]
        block_timeliness={anchor_root: [True, True]},
        checkpoint_states={justified_checkpoint: copy(anchor_state)},
        unrealized_justifications={anchor_root: justified_checkpoint},
        # [New in Gloas:EIP7732]
        payloads=set(),
        # [New in Gloas:EIP7732]
        payload_timeliness_vote={
            anchor_root: Vector[boolean, PTC_SIZE](True for _ in range(PTC_SIZE))
        },
        # [New in Gloas:EIP7732]
        payload_data_availability_vote={
            anchor_root: Vector[boolean, PTC_SIZE](True for _ in range(PTC_SIZE))
        },
    )
```

### New `notify_ptc_messages`

```python
def notify_ptc_messages(
    store: Store, state: BeaconState, payload_attestations: Sequence[PayloadAttestation]
) -> None:
    """
    Extracts a list of ``PayloadAttestationMessage`` from ``payload_attestations`` and updates the store with them
    These Payload attestations are assumed to be in the beacon block hence signature verification is not needed
    """
    if state.slot == 0:
        return
    for payload_attestation in payload_attestations:
        indexed_payload_attestation = get_indexed_payload_attestation(state, payload_attestation)
        for idx in indexed_payload_attestation.attesting_indices:
            on_payload_attestation_message(
                store,
                PayloadAttestationMessage(
                    validator_index=idx,
                    data=payload_attestation.data,
                    signature=BLSSignature(),
                ),
                is_from_block=True,
            )
```

### New `is_payload_timely`

```python
def is_payload_timely(store: Store, root: Root) -> bool:
    """
    Return whether the execution payload for the beacon block with root ``root``
    was voted as present by the PTC, and was locally determined to be available.
    """
    # The beacon block root must be known
    assert root in store.payload_timeliness_vote

    # If the payload is not locally available, the payload
    # is not considered available regardless of the PTC vote
    if root not in store.payloads:
        return False

    return sum(store.payload_timeliness_vote[root]) > PAYLOAD_TIMELY_THRESHOLD
```

### New `is_payload_data_available`

```python
def is_payload_data_available(store: Store, root: Root) -> bool:
    """
    Return whether the blob data for the beacon block with root ``root``
    was voted as present by the PTC, and was locally determined to be available.
    """
    # The beacon block root must be known
    assert root in store.payload_data_availability_vote

    # If the payload is not locally available, the blob data
    # is not considered available regardless of the PTC vote
    if root not in store.payloads:
        return False

    return sum(store.payload_data_availability_vote[root]) > DATA_AVAILABILITY_TIMELY_THRESHOLD
```

### New `can_extend_payload`

```python
def can_extend_payload(store: Store, root: Root) -> bool:
    """
    Return whether the payload for beacon block ``root`` is locally verified and
    may be used as the EL parent of a child block.
    """
    return root in store.payloads
```

### New `is_within_ptc_advisory_window`

```python
def is_within_ptc_advisory_window(store: Store, root: Root) -> bool:
    """
    Return whether the locally observed PTC view for beacon block ``root``
    remains within the reconstructible advisory window.
    """
    block_epoch = compute_epoch_at_slot(store.blocks[root].slot)
    current_epoch = get_current_store_epoch(store)
    return block_epoch + 1 >= current_epoch
```

### New `should_extend_payload`

*Note*: `should_extend_payload` returns whether fork choice prefers extending
the payload for the beacon block `root`. If `can_extend_payload(store, root)` is
`False`, the payload cannot be extended regardless of the PTC view. This PTC
preference is advisory and only applies while ``root`` remains within the
reconstructible PTC window. Once ``root`` is older than the store's previous
epoch, the prior PTC view is considered stale and no longer constrains FULL vs
EMPTY.

```python
def should_extend_payload(store: Store, root: Root) -> bool:
    return (
        can_extend_payload(store, root)
        and is_within_ptc_advisory_window(store, root)
        and is_payload_timely(store, root)
        and is_payload_data_available(store, root)
    )
```

### New `is_ptc_disrespecting_block`

```python
def is_ptc_disrespecting_block(store: Store, root: Root) -> bool:
    """
    Return whether ``root`` reorders away from a parent payload that fork choice
    says should be extended.
    """
    block = store.blocks[root]
    parent_root = block.parent_root
    parent_bid = store.blocks[parent_root].body.signed_execution_payload_bid.message
    bid = block.body.signed_execution_payload_bid.message
    return (
        should_extend_payload(store, parent_root)
        and bid.parent_block_hash != parent_bid.block_hash
    )
```

### New `should_apply_proposer_boost`

```python
def should_apply_proposer_boost(store: Store) -> bool:
    if store.proposer_boost_root == Root():
        return False

    block = store.blocks[store.proposer_boost_root]
    parent_root = block.parent_root
    parent = store.blocks[parent_root]
    slot = block.slot

    # Apply proposer boost if `parent` is not from the previous slot
    if parent.slot + 1 < slot:
        return True

    # Apply proposer boost if `parent` is not weak
    if not is_head_weak(store, parent_root):
        return True

    # If `parent` is weak and from the previous slot, apply
    # proposer boost if there are no early equivocations
    equivocations = [
        root
        for root, block in store.blocks.items()
        if (
            store.block_timeliness[root][PTC_TIMELINESS_INDEX]
            and block.proposer_index == parent.proposer_index
            and block.slot + 1 == slot
            and root != parent_root
        )
    ]

    return len(equivocations) == 0
```

### Modified `get_weight`

```python
def get_weight(store: Store, root: Root) -> Gwei:
    state = store.checkpoint_states[store.justified_checkpoint]
    attestation_score = get_attestation_score(store, root, state)
    # [Modified in Gloas:EIP7732]
    if not should_apply_proposer_boost(store):
        # Return only attestation score if
        # proposer boost should not apply
        return attestation_score

    # Calculate proposer score if ``proposer_boost_root`` is set
    proposer_score = Gwei(0)
    # Boost is applied if ``root`` is an ancestor of ``proposer_boost_root``
    if get_ancestor(store, store.proposer_boost_root, store.blocks[root].slot) == root:
        proposer_score = get_proposer_score(store)
    return attestation_score + proposer_score
```

### Modified `get_head`

*Note*: `get_head` is modified to avoid descending into a child that reorders
away from a parent payload which fork choice says should be extended. If all
children of the current head are filtered out in this way, the parent remains
the head.

```python
def get_head(store: Store) -> Root:
    # Get filtered block tree that only includes viable branches
    blocks = get_filtered_block_tree(store)
    # Execute the LMD-GHOST fork choice
    head = store.justified_checkpoint.root
    while True:
        children = [root for root in blocks.keys() if blocks[root].parent_root == head]
        children = [root for root in children if not is_ptc_disrespecting_block(store, root)]
        if len(children) == 0:
            return head
        # Sort by latest attesting balance with ties broken lexicographically
        # Ties broken by favoring block with lexicographically higher root
        head = max(children, key=lambda root: (get_weight(store, root), root))
```

### Modified `record_block_timeliness`

```python
def record_block_timeliness(store: Store, root: Root) -> None:
    block = store.blocks[root]
    seconds_since_genesis = store.time - store.genesis_time
    time_into_slot_ms = seconds_to_milliseconds(seconds_since_genesis) % SLOT_DURATION_MS
    epoch = get_current_store_epoch(store)
    attestation_threshold_ms = get_attestation_due_ms(epoch)
    # [New in Gloas:EIP7732]
    is_current_slot = get_current_slot(store) == block.slot
    ptc_threshold_ms = get_payload_attestation_due_ms(epoch)
    # [Modified in Gloas:EIP7732]
    store.block_timeliness[root] = [
        is_current_slot and time_into_slot_ms < threshold
        for threshold in [attestation_threshold_ms, ptc_threshold_ms]
    ]
```

### Modified `update_proposer_boost_root`

```python
def update_proposer_boost_root(store: Store, root: Root) -> None:
    is_first_block = store.proposer_boost_root == Root()
    # [Modified in Gloas:EIP7732]
    is_timely = store.block_timeliness[root][ATTESTATION_TIMELINESS_INDEX]

    # Add proposer score boost if the block is the first timely block
    # for this slot, with the same proposer as the canonical chain.
    if is_timely and is_first_block:
        head_state = copy(store.block_states[get_head(store)])
        slot = get_current_slot(store)
        if head_state.slot < slot:
            process_slots(head_state, slot)
        block = store.blocks[root]
        # Only update if the proposer is the same as on the canonical chain
        if block.proposer_index == get_beacon_proposer_index(head_state):
            store.proposer_boost_root = root
```

### Modified `is_head_late`

*Note*: The only change is that `store.block_timeliness[root]` now records
timeliness with respect to two different deadlines. `is_head_late` takes into
account timeliness with respect to the attestation deadline, which is retrieved
at `ATTESTATION_TIMELINESS_INDEX`.

```python
def is_head_late(store: Store, head_root: Root) -> bool:
    return not store.block_timeliness[head_root][ATTESTATION_TIMELINESS_INDEX]
```

### Modified `is_head_weak`

*Note*: The function `is_head_weak` now also counts weight from equivocating
validators from the committees of the head slot. This ensures that the counted
weight and the output of `is_head_weak` are monotonic: more attestations can
only increase the weight and change the output from `True` to `False`, not
vice-versa.

```python
def is_head_weak(store: Store, head_root: Root) -> bool:
    # Calculate weight threshold for weak head
    justified_state = store.checkpoint_states[store.justified_checkpoint]
    reorg_threshold = calculate_committee_fraction(justified_state, REORG_HEAD_WEIGHT_THRESHOLD)

    # Compute head weight including equivocations
    head_state = store.block_states[head_root]
    head_block = store.blocks[head_root]
    epoch = compute_epoch_at_slot(head_block.slot)
    head_weight = get_attestation_score(store, head_root, justified_state)
    for index in range(get_committee_count_per_slot(head_state, epoch)):
        committee = get_beacon_committee(head_state, head_block.slot, CommitteeIndex(index))
        head_weight += Gwei(
            sum(
                justified_state.validators[i].effective_balance
                for i in committee
                if i in store.equivocating_indices
            )
        )

    return head_weight < reorg_threshold
```

### Modified `get_attestation_due_ms`

```python
def get_attestation_due_ms(epoch: Epoch) -> uint64:
    # [New in Gloas]
    if epoch >= GLOAS_FORK_EPOCH:
        return get_slot_component_duration_ms(ATTESTATION_DUE_BPS_GLOAS)
    return get_slot_component_duration_ms(ATTESTATION_DUE_BPS)
```

### Modified `get_aggregate_due_ms`

```python
def get_aggregate_due_ms(epoch: Epoch) -> uint64:
    # [New in Gloas]
    if epoch >= GLOAS_FORK_EPOCH:
        return get_slot_component_duration_ms(AGGREGATE_DUE_BPS_GLOAS)
    return get_slot_component_duration_ms(AGGREGATE_DUE_BPS)
```

### Modified `get_sync_message_due_ms`

```python
def get_sync_message_due_ms(epoch: Epoch) -> uint64:
    # [New in Gloas]
    if epoch >= GLOAS_FORK_EPOCH:
        return get_slot_component_duration_ms(SYNC_MESSAGE_DUE_BPS_GLOAS)
    return get_slot_component_duration_ms(SYNC_MESSAGE_DUE_BPS)
```

### Modified `get_contribution_due_ms`

```python
def get_contribution_due_ms(epoch: Epoch) -> uint64:
    # [New in Gloas]
    if epoch >= GLOAS_FORK_EPOCH:
        return get_slot_component_duration_ms(CONTRIBUTION_DUE_BPS_GLOAS)
    return get_slot_component_duration_ms(CONTRIBUTION_DUE_BPS)
```

### New `get_payload_attestation_due_ms`

```python
def get_payload_attestation_due_ms(epoch: Epoch) -> uint64:
    return get_slot_component_duration_ms(PAYLOAD_ATTESTATION_DUE_BPS)
```

## Handlers

### Modified `on_block`

*Note*: The handler `on_block` is modified to validate the parent payload hash
and delay blob data availability checking until the processing of the execution
payload.

```python
def on_block(store: Store, signed_block: SignedBeaconBlock) -> None:
    """
    Run ``on_block`` upon receiving a new block.
    """
    block = signed_block.message
    # Parent block must be known
    assert block.parent_root in store.block_states

    # [New in Gloas:EIP7732]
    # Validate parent payload hash
    bid = block.body.signed_execution_payload_bid.message
    parent = store.blocks[block.parent_root]
    parent_bid = parent.body.signed_execution_payload_bid.message
    if bid.parent_block_hash == parent_bid.block_hash:
        # Building on parent's full payload -- payload must have been
        # verified by on_execution_payload
        assert block.parent_root in store.payloads
    else:
        # Not building on parent's payload -- must continue from the
        # same EL chain tip as the parent
        assert bid.parent_block_hash == parent_bid.parent_block_hash

    # Blocks cannot be in the future. If they are, their consideration must be delayed until they are in the past.
    current_slot = get_current_slot(store)
    assert current_slot >= block.slot

    # Check that block is later than the finalized epoch slot (optimization to reduce calls to get_ancestor)
    finalized_slot = compute_start_slot_at_epoch(store.finalized_checkpoint.epoch)
    assert block.slot > finalized_slot
    # Check block is a descendant of the finalized block at the checkpoint finalized slot
    finalized_checkpoint_block = get_checkpoint_block(
        store,
        block.parent_root,
        store.finalized_checkpoint.epoch,
    )
    assert store.finalized_checkpoint.root == finalized_checkpoint_block

    # Make a copy of the state to avoid mutability issues
    state = copy(store.block_states[block.parent_root])

    # Check the block is valid and compute the post-state
    block_root = hash_tree_root(block)
    state_transition(state, signed_block, True)

    # Add new block to the store
    store.blocks[block_root] = block
    # Add new state for this block to the store
    store.block_states[block_root] = state
    # Add a new PTC voting for this block to the store
    store.payload_timeliness_vote[block_root] = [False] * PTC_SIZE
    store.payload_data_availability_vote[block_root] = [False] * PTC_SIZE

    # Notify the store about the payload_attestations in the block
    notify_ptc_messages(store, state, block.body.payload_attestations)

    record_block_timeliness(store, block_root)
    update_proposer_boost_root(store, block_root)

    # Update checkpoints in store if necessary
    update_checkpoints(store, state.current_justified_checkpoint, state.finalized_checkpoint)

    # Eagerly compute unrealized justification and finality.
    compute_pulled_up_tip(store, block_root)
```

### Modified `is_data_available`

```python
def is_data_available(beacon_block_root: Root) -> bool:
    # `retrieve_column_sidecars_and_kzg_commitments` is implementation and
    # context dependent, replacing `retrieve_column_sidecars`. For the given
    # block root, it returns all column sidecars to sample, or raises an
    # exception if they are not available, in addition it returns all the
    # corresponding kzg commitments. The p2p network does not guarantee sidecar
    # retrieval outside of `MIN_EPOCHS_FOR_DATA_COLUMN_SIDECARS_REQUESTS` epochs.
    column_sidecars, kzg_commitments = retrieve_column_sidecars_and_kzg_commitments(
        beacon_block_root
    )
    return all(
        verify_data_column_sidecar(column_sidecar, kzg_commitments)
        and verify_data_column_sidecar_kzg_proofs(column_sidecar, kzg_commitments)
        for column_sidecar in column_sidecars
    )
```

### New `on_execution_payload`

The handler `on_execution_payload` is called when the node receives a
`SignedExecutionPayloadEnvelope` to sync.

```python
def on_execution_payload(store: Store, signed_envelope: SignedExecutionPayloadEnvelope) -> None:
    """
    Run ``on_execution_payload`` upon receiving a new execution payload.
    """
    envelope = signed_envelope.message
    # The corresponding beacon block root needs to be known
    assert envelope.beacon_block_root in store.block_states

    # Check if blob data is available
    # If not, this payload MAY be queued and subsequently considered when blob data becomes available
    assert is_data_available(envelope.beacon_block_root)

    state = store.block_states[envelope.beacon_block_root]

    # Process the execution payload
    process_execution_payload(state, signed_envelope, EXECUTION_ENGINE)

    # Mark this block's execution payload as verified
    store.payloads.add(envelope.beacon_block_root)
```

### New `on_payload_attestation_message`

```python
def on_payload_attestation_message(
    store: Store, ptc_message: PayloadAttestationMessage, is_from_block: bool = False
) -> None:
    """
    Run ``on_payload_attestation_message`` upon receiving a new ``ptc_message`` from
    either within a block or directly on the wire.
    """
    # The beacon block root must be known
    data = ptc_message.data
    # PTC attestation must be for a known block. If block is unknown, delay consideration until the block is found
    assert data.beacon_block_root in store.block_states
    state = store.block_states[data.beacon_block_root]
    ptc = get_ptc(state, data.slot)
    # PTC votes can only change the vote for their assigned beacon block, return early otherwise
    if data.slot != state.slot:
        return
    # Check that the attester is from the PTC
    assert ptc_message.validator_index in ptc

    # Verify the signature and check that its for the current slot if it is coming from the wire
    if not is_from_block:
        # Check that the attestation is for the current slot
        assert data.slot == get_current_slot(store)
        # Verify the signature
        assert is_valid_indexed_payload_attestation(
            state,
            IndexedPayloadAttestation(
                attesting_indices=[ptc_message.validator_index],
                data=data,
                signature=ptc_message.signature,
            ),
        )
    # Update the votes for the block
    ptc_index = ptc.index(ptc_message.validator_index)
    payload_timeliness_vote = store.payload_timeliness_vote[data.beacon_block_root]
    payload_timeliness_vote[ptc_index] = data.payload_present
    payload_data_availability_vote = store.payload_data_availability_vote[data.beacon_block_root]
    payload_data_availability_vote[ptc_index] = data.blob_data_available
```
