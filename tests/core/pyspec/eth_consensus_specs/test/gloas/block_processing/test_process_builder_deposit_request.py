from eth_consensus_specs.test.context import always_bls, spec_state_test, with_gloas_and_later
from eth_consensus_specs.test.helpers.keys import builder_pubkeys, pubkey_to_privkey
from tests.infra.helpers.deposit_requests import (
    assert_process_builder_deposit_request,
    prepare_process_builder_deposit_request,
    run_builder_deposit_request_processing,
)

#
# New builder deposits
#


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__new_builder(spec, state):
    """Test fresh builder deposit creates a new builder."""
    amount = spec.MIN_DEPOSIT_AMOUNT
    request = prepare_process_builder_deposit_request(spec, state, amount=amount, signed=True)
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_balance=amount,
        expected_builder_withdrawable_epoch=spec.FAR_FUTURE_EPOCH,
        slot_reused=False,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__new_builder_large_amount(spec, state):
    """Test fresh builder deposit with a large amount."""
    amount = spec.MAX_EFFECTIVE_BALANCE_ELECTRA
    request = prepare_process_builder_deposit_request(spec, state, amount=amount, signed=True)
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_balance=amount,
        slot_reused=False,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__new_builder_below_el_minimum(spec, state):
    """Test that the consensus layer does not re-assert the EL deposit minimum.

    The 1-ETH minimum is enforced only by the builder deposit contract; a
    smaller amount in a record still registers a builder.
    """
    amount = spec.Gwei(1)
    request = prepare_process_builder_deposit_request(spec, state, amount=amount, signed=True)
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_balance=amount,
        slot_reused=False,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__new_builder_execution_address(spec, state):
    """Test that the new builder's execution address comes from the credentials."""
    execution_address = b"\x42" * 20
    withdrawal_credentials = spec.BUILDER_WITHDRAWAL_PREFIX + b"\x00" * 11 + execution_address
    request = prepare_process_builder_deposit_request(
        spec,
        state,
        amount=spec.MIN_DEPOSIT_AMOUNT,
        signed=True,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_execution_address=execution_address,
        slot_reused=False,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__new_builder_existing_validator_pubkey(spec, state):
    """Test that a pubkey already registered as a validator can register a builder.

    The validator and builder registries are keyed independently (EIP-8282);
    the same public key may be both.
    """
    validator_pubkey = state.validators[0].pubkey
    privkey = pubkey_to_privkey[validator_pubkey]
    amount = spec.MIN_DEPOSIT_AMOUNT

    request = prepare_process_builder_deposit_request(
        spec, state, pubkey=validator_pubkey, privkey=privkey, amount=amount, signed=True
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_balance=amount,
        slot_reused=False,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__new_builder_empty_registry(spec, state):
    """Test builder deposit into an empty builder registry."""
    state.builders = []
    amount = spec.MIN_DEPOSIT_AMOUNT
    request = prepare_process_builder_deposit_request(spec, state, amount=amount, signed=True)
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_count=1,
        expected_builder_index=0,
        expected_builder_balance=amount,
    )


#
# Ignored first deposits
#


@with_gloas_and_later
@spec_state_test
@always_bls
def test_process_builder_deposit_request__ignored_invalid_signature(spec, state):
    """Test that a first deposit with an invalid proof-of-possession is ignored."""
    request = prepare_process_builder_deposit_request(
        spec, state, amount=spec.MIN_DEPOSIT_AMOUNT, signed=False
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        state_unchanged=True,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__ignored_non_builder_credentials(spec, state):
    """Test that a first deposit without the builder credential prefix is ignored."""
    new_pubkey = builder_pubkeys[len(state.builders)]
    withdrawal_credentials = spec.ETH1_ADDRESS_WITHDRAWAL_PREFIX + b"\x00" * 11 + b"\x42" * 20
    request = prepare_process_builder_deposit_request(
        spec,
        state,
        pubkey=new_pubkey,
        amount=spec.MIN_DEPOSIT_AMOUNT,
        signed=True,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        state_unchanged=True,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__ignored_bls_credentials(spec, state):
    """Test that a first deposit with BLS (0x00) credentials is ignored."""
    new_pubkey = builder_pubkeys[len(state.builders)]
    withdrawal_credentials = spec.BLS_WITHDRAWAL_PREFIX + spec.hash(new_pubkey)[1:]
    request = prepare_process_builder_deposit_request(
        spec,
        state,
        pubkey=new_pubkey,
        amount=spec.MIN_DEPOSIT_AMOUNT,
        signed=True,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        state_unchanged=True,
    )


#
# Top-up deposits for existing builders
#


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__top_up(spec, state):
    """Test top-up deposit for an existing builder."""
    pre_builder_count = len(state.builders)
    amount = spec.MIN_DEPOSIT_AMOUNT
    request = prepare_process_builder_deposit_request(
        spec, state, builder_index=0, amount=amount, signed=True
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_count=pre_builder_count,
        expected_builder_balance_delta=amount,
    )


@with_gloas_and_later
@spec_state_test
@always_bls
def test_process_builder_deposit_request__top_up_invalid_signature(spec, state):
    """Test that a top-up's signature is ignored.

    The proof-of-possession is checked only on a pubkey's first appearance;
    later deposits are stake additions.
    """
    pre_builder_count = len(state.builders)
    amount = spec.MIN_DEPOSIT_AMOUNT
    request = prepare_process_builder_deposit_request(
        spec, state, builder_index=0, amount=amount, signed=False
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_count=pre_builder_count,
        expected_builder_balance_delta=amount,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__top_up_different_credentials(spec, state):
    """Test that a top-up cannot redirect the builder's withdrawal target.

    The supplied withdrawal_credentials are ignored for an existing builder;
    the registration is unchanged.
    """
    pre_execution_address = state.builders[0].execution_address
    amount = spec.MIN_DEPOSIT_AMOUNT
    withdrawal_credentials = spec.BUILDER_WITHDRAWAL_PREFIX + b"\x00" * 11 + b"\x66" * 20
    assert spec.ExecutionAddress(withdrawal_credentials[12:]) != pre_execution_address

    request = prepare_process_builder_deposit_request(
        spec,
        state,
        builder_index=0,
        amount=amount,
        signed=True,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_balance_delta=amount,
        expected_execution_address=pre_execution_address,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__top_up_exited_builder(spec, state):
    """Test that a deposit to an exited builder is a top-up and does not reactivate it."""
    withdrawable_epoch = spec.get_current_epoch(state) + 10
    state.builders[0].withdrawable_epoch = withdrawable_epoch
    amount = spec.MIN_DEPOSIT_AMOUNT

    request = prepare_process_builder_deposit_request(
        spec, state, builder_index=0, amount=amount, signed=True
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        expected_builder_balance_delta=amount,
        expected_builder_withdrawable_epoch=withdrawable_epoch,
    )


#
# Builder index reuse
#


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__reuses_exited_builder_slot(spec, state):
    """Test that a new builder can reuse the slot of a fully exited builder."""
    pre_builder_count = len(state.builders)

    request = prepare_process_builder_deposit_request(
        spec,
        state,
        amount=spec.MIN_DEPOSIT_AMOUNT,
        signed=True,
        advance_epochs=1,
        builder_modifications={0: {"withdrawable_epoch": "current_epoch-1", "balance": 0}},
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        slot_reused=True,
        expected_builder_count=pre_builder_count,
        expected_builder_index=0,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__no_reuse_future_epoch(spec, state):
    """Test that a slot is not reused while its withdrawable epoch is in the future."""
    request = prepare_process_builder_deposit_request(
        spec,
        state,
        amount=spec.MIN_DEPOSIT_AMOUNT,
        signed=True,
        advance_epochs=1,
        builder_modifications={0: {"withdrawable_epoch": "current_epoch+1", "balance": 0}},
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        slot_reused=False,
    )


@with_gloas_and_later
@spec_state_test
def test_process_builder_deposit_request__no_reuse_nonzero_balance(spec, state):
    """Test that a slot is not reused while the exited builder still has balance."""
    request = prepare_process_builder_deposit_request(
        spec,
        state,
        amount=spec.MIN_DEPOSIT_AMOUNT,
        signed=True,
        advance_epochs=1,
        builder_modifications={0: {"withdrawable_epoch": "current_epoch-1", "balance": 1}},
    )
    pre_state = state.copy()

    yield from run_builder_deposit_request_processing(spec, state, request)

    assert_process_builder_deposit_request(
        spec,
        state,
        pre_state,
        builder_deposit_request=request,
        slot_reused=False,
    )
