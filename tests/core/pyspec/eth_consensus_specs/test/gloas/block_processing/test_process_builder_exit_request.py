from eth_consensus_specs.test.context import spec_state_test, with_gloas_and_later
from eth_consensus_specs.test.helpers.state import next_slots
from tests.infra.helpers.deposit_requests import run_builder_exit_request_processing


def advance_past_finalization(spec, state):
    """Advance slots and finalize so that genesis-epoch builders become active."""
    epoch = spec.get_current_epoch(state)
    next_slots(spec, state, spec.SLOTS_PER_EPOCH * 3)
    state.finalized_checkpoint.epoch = epoch + 1


def make_exit_request(spec, state, builder_index, source_address=None):
    """Build a BuilderExitRequest for the given builder."""
    builder = state.builders[builder_index]
    if source_address is None:
        source_address = builder.execution_address
    return spec.BuilderExitRequest(
        source_address=source_address,
        pubkey=builder.pubkey,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_exit_request__success(spec, state):
    """Test successful builder exit request with no pending balance."""
    builder_index = 0
    advance_past_finalization(spec, state)
    assert spec.is_active_builder(state, builder_index)
    assert spec.get_pending_balance_to_withdraw_for_builder(state, builder_index) == 0

    current_epoch = spec.get_current_epoch(state)
    exit_request = make_exit_request(spec, state, builder_index)

    yield from run_builder_exit_request_processing(spec, state, exit_request)

    assert not spec.is_active_builder(state, builder_index)
    expected_withdrawable = current_epoch + spec.config.MIN_BUILDER_WITHDRAWABILITY_DELAY
    assert state.builders[builder_index].withdrawable_epoch == expected_withdrawable


@with_gloas_and_later
@spec_state_test
def test_process_builder_exit_request__ignored_unknown_pubkey(spec, state):
    """Test that an exit request for an unknown pubkey is ignored."""
    advance_past_finalization(spec, state)

    exit_request = spec.BuilderExitRequest(
        source_address=spec.ExecutionAddress(b"\x42" * 20),
        pubkey=spec.BLSPubkey(b"\xab" * 48),
    )
    pre_state = state.copy()

    yield from run_builder_exit_request_processing(spec, state, exit_request)

    assert state == pre_state


@with_gloas_and_later
@spec_state_test
def test_process_builder_exit_request__ignored_incorrect_source_address(spec, state):
    """Test that an exit request from an address other than the builder's is ignored.

    The source address is the sole authorization of a builder exit request.
    """
    builder_index = 0
    advance_past_finalization(spec, state)
    assert spec.is_active_builder(state, builder_index)

    wrong_address = spec.ExecutionAddress(b"\x42" * 20)
    assert state.builders[builder_index].execution_address != wrong_address
    exit_request = make_exit_request(spec, state, builder_index, source_address=wrong_address)
    pre_state = state.copy()

    yield from run_builder_exit_request_processing(spec, state, exit_request)

    assert state == pre_state
    assert spec.is_active_builder(state, builder_index)


@with_gloas_and_later
@spec_state_test
def test_process_builder_exit_request__ignored_inactive_deposit_epoch(spec, state):
    """Test that an inactive builder (deposit epoch not finalized) cannot exit."""
    builder_index = 0

    # Set builder's deposit epoch to a non-finalized epoch
    state.builders[builder_index].deposit_epoch = spec.Epoch(1)

    advance_past_finalization(spec, state)
    assert state.finalized_checkpoint.epoch == state.builders[builder_index].deposit_epoch
    assert not spec.is_active_builder(state, builder_index)

    exit_request = make_exit_request(spec, state, builder_index)
    pre_state = state.copy()

    yield from run_builder_exit_request_processing(spec, state, exit_request)

    assert state == pre_state


@with_gloas_and_later
@spec_state_test
def test_process_builder_exit_request__ignored_already_exited(spec, state):
    """Test that an already-exited builder cannot exit again."""
    builder_index = 0

    # Set builder's withdrawable epoch which indicates it has initiated an exit
    withdrawable_epoch = spec.get_current_epoch(state) + 10
    state.builders[builder_index].withdrawable_epoch = withdrawable_epoch

    advance_past_finalization(spec, state)
    assert not spec.is_active_builder(state, builder_index)

    exit_request = make_exit_request(spec, state, builder_index)
    pre_state = state.copy()

    yield from run_builder_exit_request_processing(spec, state, exit_request)

    assert state == pre_state
    assert state.builders[builder_index].withdrawable_epoch == withdrawable_epoch


@with_gloas_and_later
@spec_state_test
def test_process_builder_exit_request__ignored_pending_withdrawal(spec, state):
    """Test that a builder cannot exit while it has a pending withdrawal."""
    builder_index = 0
    advance_past_finalization(spec, state)
    assert spec.is_active_builder(state, builder_index)

    # Add pending withdrawal for this builder
    withdrawal_amount = spec.MIN_ACTIVATION_BALANCE
    withdrawal = spec.BuilderPendingWithdrawal(
        fee_recipient=spec.ExecutionAddress(b"\x70" * 20),
        amount=withdrawal_amount,
        builder_index=builder_index,
    )
    state.builder_pending_withdrawals.append(withdrawal)
    pending_balance = spec.get_pending_balance_to_withdraw_for_builder(state, builder_index)
    assert pending_balance == withdrawal_amount

    exit_request = make_exit_request(spec, state, builder_index)
    pre_state = state.copy()

    yield from run_builder_exit_request_processing(spec, state, exit_request)

    assert state == pre_state
    assert spec.is_active_builder(state, builder_index)


@with_gloas_and_later
@spec_state_test
def test_process_builder_exit_request__ignored_pending_payment(spec, state):
    """Test that a builder cannot exit while it has a pending payment."""
    builder_index = 0
    advance_past_finalization(spec, state)
    assert spec.is_active_builder(state, builder_index)

    # Add pending payment for this builder
    payment_amount = spec.MIN_ACTIVATION_BALANCE
    payment = spec.BuilderPendingPayment(
        weight=spec.get_builder_payment_quorum_threshold(state) + 1,
        withdrawal=spec.BuilderPendingWithdrawal(
            fee_recipient=spec.ExecutionAddress(b"\x60" * 20),
            amount=payment_amount,
            builder_index=builder_index,
        ),
    )
    state.builder_pending_payments[0] = payment
    pending_balance = spec.get_pending_balance_to_withdraw_for_builder(state, builder_index)
    assert pending_balance == payment_amount

    exit_request = make_exit_request(spec, state, builder_index)
    pre_state = state.copy()

    yield from run_builder_exit_request_processing(spec, state, exit_request)

    assert state == pre_state
    assert spec.is_active_builder(state, builder_index)
