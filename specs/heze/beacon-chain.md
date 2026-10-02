# Heze -- The Beacon Chain

*Note*: This document is a work-in-progress for researchers and implementers.

<!-- mdformat-toc start --slug=github --no-anchors --maxlevel=6 --minlevel=2 -->

- [Introduction](#introduction)
- [Types](#types)
  - [New `InclusionListBits`](#new-inclusionlistbits)
  - [New `InclusionListCommittee`](#new-inclusionlistcommittee)
- [Constants](#constants)
  - [Domains](#domains)
- [Presets](#presets)
  - [Inclusion list committee](#inclusion-list-committee)
- [Configs](#configs)
  - [Time parameters](#time-parameters)
- [Containers](#containers)
  - [New containers](#new-containers)
    - [`InclusionList`](#inclusionlist)
    - [`SignedInclusionList`](#signedinclusionlist)
  - [Modified containers](#modified-containers)
    - [`ExecutionPayloadBid`](#executionpayloadbid)
    - [`SignedExecutionPayloadBid`](#signedexecutionpayloadbid)
    - [`BeaconState`](#beaconstate)
- [Helpers](#helpers)
  - [Misc](#misc)
    - [New `get_slot_schedule`](#new-get_slot_schedule)
    - [New `get_slot_duration_ms`](#new-get_slot_duration_ms)
    - [Modified `compute_time_at_slot_ms`](#modified-compute_time_at_slot_ms)
    - [Modified `compute_slot_at_time_ms`](#modified-compute_slot_at_time_ms)
  - [Predicates](#predicates)
    - [New `is_valid_inclusion_list_signature`](#new-is_valid_inclusion_list_signature)
  - [Beacon state accessors](#beacon-state-accessors)
    - [New `get_inclusion_list_committee`](#new-get_inclusion_list_committee)

<!-- mdformat-toc end -->

## Introduction

Heze is a consensus-layer upgrade containing a number of features. Including:

- [EIP-7805](https://github.com/ethereum/EIPs/blob/9a345f96c2295a678b0ce33e94d41276ddb3fdef/EIPS/eip-7805.md):
  Fork-choice enforced Inclusion Lists (FOCIL)

*Note*: These EIPs are in draft and may change or be removed. Each link above
points to the specific version targeted by this specification, which may differ
from the latest published version of the EIPs.

## Types

### New `InclusionListBits`

```python
class InclusionListBits(BitVector):
    """
    A bitfield over the inclusion list committee, one bit per member in
    committee order.
    """

    LENGTH = INCLUSION_LIST_COMMITTEE_SIZE
```

### New `InclusionListCommittee`

```python
class InclusionListCommittee(Vector[ValidatorIndex]):
    """
    The inclusion list committee of a slot.
    """

    LENGTH = INCLUSION_LIST_COMMITTEE_SIZE
```

## Constants

### Domains

| Name                              | Value                      |
| --------------------------------- | -------------------------- |
| `DOMAIN_INCLUSION_LIST_COMMITTEE` | `DomainType("0x10000000")` |

## Presets

### Inclusion list committee

| Name                            | Value                 |
| ------------------------------- | --------------------- |
| `INCLUSION_LIST_COMMITTEE_SIZE` | `Uint64(2**4)` (= 16) |

## Configs

### Time parameters

| Name                    | Value           |
| ----------------------- | --------------- |
| `SLOT_DURATION_MS_HEZE` | `Uint64(12000)` |

## Containers

### New containers

#### `InclusionList`

```python
class InclusionList(Container):
    slot: Slot
    validator_index: ValidatorIndex
    dependent_root: Root
    transactions: Transactions
```

#### `SignedInclusionList`

```python
class SignedInclusionList(Container):
    message: InclusionList
    signature: BLSSignature
```

### Modified containers

#### `ExecutionPayloadBid`

```python
class ExecutionPayloadBid(ProgressiveContainer):
    ACTIVE_FIELDS = active_fields(width=13)

    parent_block_hash: Hash32
    parent_block_root: Root
    block_hash: Hash32
    prev_randao: Bytes32
    fee_recipient: ExecutionAddress
    gas_limit: Uint64
    builder_index: BuilderIndex
    slot: Slot
    value: Gwei
    execution_payment: Gwei
    blob_kzg_commitments: BlobKZGCommitments
    execution_requests_root: Root
    # [New in Heze:EIP7805]
    inclusion_list_bits: InclusionListBits
```

#### `SignedExecutionPayloadBid`

```python
class SignedExecutionPayloadBid(Container):
    # [Modified in Heze:EIP7805]
    message: ExecutionPayloadBid
    signature: BLSSignature
```

#### `BeaconState`

```python
class BeaconState(ProgressiveContainer):
    ACTIVE_FIELDS = active_fields(width=46)

    genesis_time: Uint64
    genesis_validators_root: Root
    slot: Slot
    fork: Fork
    latest_block_header: BeaconBlockHeader
    block_roots: BlockRoots
    state_roots: StateRoots
    historical_roots: HistoricalRoots
    eth1_data: Eth1Data
    eth1_data_votes: Eth1DataVotes
    eth1_deposit_index: Uint64
    validators: Validators
    balances: Balances
    randao_mixes: RandaoMixes
    slashings: Slashings
    previous_epoch_participation: EpochParticipation
    current_epoch_participation: EpochParticipation
    justification_bits: JustificationBits
    previous_justified_checkpoint: Checkpoint
    current_justified_checkpoint: Checkpoint
    finalized_checkpoint: Checkpoint
    inactivity_scores: InactivityScores
    current_sync_committee: SyncCommittee
    next_sync_committee: SyncCommittee
    latest_block_hash: Hash32
    next_withdrawal_index: WithdrawalIndex
    next_withdrawal_validator_index: ValidatorIndex
    historical_summaries: HistoricalSummaries
    deposit_requests_start_index: Uint64
    deposit_balance_to_consume: Gwei
    exit_balance_to_consume: Gwei
    earliest_exit_epoch: Epoch
    consolidation_balance_to_consume: Gwei
    earliest_consolidation_epoch: Epoch
    pending_deposits: PendingDeposits
    pending_partial_withdrawals: PendingPartialWithdrawals
    pending_consolidations: PendingConsolidations
    proposer_lookahead: ProposerLookahead
    builders: Builders
    next_withdrawal_builder_index: BuilderIndex
    execution_payload_availability: ExecutionPayloadAvailability
    builder_pending_payments: BuilderPendingPayments
    builder_pending_withdrawals: BuilderPendingWithdrawals
    # [Modified in Heze:EIP7805]
    latest_execution_payload_bid: ExecutionPayloadBid
    payload_expected_withdrawals: Withdrawals
    ptc_window: PayloadTimelinessCommitteeWindow
```

## Helpers

### Misc

#### New `get_slot_schedule`

```python
def get_slot_schedule() -> Sequence[dict[str, Uint64]]:
    """
    Return the slot duration schedule derived from fork configuration.
    """
    return [
        {"EPOCH": GENESIS_EPOCH, "SLOT_MS": SLOT_DURATION_MS},
        {"EPOCH": HEZE_FORK_EPOCH, "SLOT_MS": SLOT_DURATION_MS_HEZE},
    ]
```

#### New `get_slot_duration_ms`

```python
def get_slot_duration_ms(epoch: Epoch) -> Uint64:
    """
    Return the slot duration in effect at ``epoch``.
    """
    for entry in reversed(get_slot_schedule()):
        if epoch >= entry["EPOCH"]:
            break
    return entry["SLOT_MS"]
```

#### Modified `compute_time_at_slot_ms`

```python
def compute_time_at_slot_ms(genesis_time_ms: Uint64, slot: Slot) -> Uint64:
    """
    Return the Unix time in milliseconds at the start of ``slot``.
    """
    # [Modified in Heze]
    end_slot = slot
    time_ms = genesis_time_ms
    for entry in reversed(get_slot_schedule()):
        if entry["EPOCH"] > compute_epoch_at_slot(end_slot):
            continue
        entry_slot = compute_start_slot_at_epoch(Epoch(entry["EPOCH"]))
        time_ms += (end_slot - entry_slot) * entry["SLOT_MS"]
        end_slot = entry_slot
    return Uint64(time_ms)
```

#### Modified `compute_slot_at_time_ms`

```python
def compute_slot_at_time_ms(genesis_time_ms: Uint64, time_ms: Uint64) -> Slot:
    """
    Return the slot at Unix time ``time_ms``.
    """
    # [Modified in Heze]
    schedule = get_slot_schedule()
    slot = GENESIS_SLOT
    time_diff_ms = time_ms - genesis_time_ms
    for index, entry in enumerate(schedule):
        slots = time_diff_ms // entry["SLOT_MS"]
        if index + 1 == len(schedule):
            break
        next_epoch = Epoch(schedule[index + 1]["EPOCH"])
        if compute_epoch_at_slot(Slot(slot + slots)) < next_epoch:
            break
        next_slot = compute_start_slot_at_epoch(next_epoch)
        time_diff_ms -= (next_slot - slot) * entry["SLOT_MS"]
        slot = next_slot
    return Slot(slot + slots)
```

### Predicates

#### New `is_valid_inclusion_list_signature`

```python
def is_valid_inclusion_list_signature(
    state: BeaconState, signed_inclusion_list: SignedInclusionList
) -> bool:
    """
    Check if ``signed_inclusion_list`` has a valid signature.
    """
    message = signed_inclusion_list.message
    index = message.validator_index
    pubkey = state.validators[index].pubkey
    domain = get_domain(state, DOMAIN_INCLUSION_LIST_COMMITTEE, compute_epoch_at_slot(message.slot))
    signing_root = compute_signing_root(message, domain)
    return bls.Verify(pubkey, signing_root, signed_inclusion_list.signature)
```

### Beacon state accessors

#### New `get_inclusion_list_committee`

```python
def get_inclusion_list_committee(state: BeaconState, slot: Slot) -> InclusionListCommittee:
    """
    Get the inclusion list committee for the given ``slot``.
    """
    epoch = compute_epoch_at_slot(slot)
    indices: list[ValidatorIndex] = []
    # Concatenate all committees for this slot in order
    committees_per_slot = get_committee_count_per_slot(state, epoch)
    for i in range(committees_per_slot):
        committee = get_beacon_committee(state, slot, CommitteeIndex(i))
        indices.extend(committee)
    return InclusionListCommittee(
        data=[indices[i % len(indices)] for i in range(INCLUSION_LIST_COMMITTEE_SIZE)]
    )
```
