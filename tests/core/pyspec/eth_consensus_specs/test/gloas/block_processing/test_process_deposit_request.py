from eth_consensus_specs.test.context import spec_state_test, with_gloas_and_later
from eth_consensus_specs.test.helpers.keys import pubkeys
from tests.infra.helpers.deposit_requests import (
    assert_process_deposit_request,
    prepare_process_deposit_request,
    run_deposit_request_processing,
)

#
# Builder-credentialed deposits are dropped (EIP-8282)
#
# Builders are created and topped up only via BUILDER_DEPOSIT_REQUEST_TYPE;
# a deposit-contract deposit committing to a BUILDER_WITHDRAWAL_PREFIX
# credential is inert: not appended to pending_deposits, no builder touched.
#


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__builder_credentials_dropped(spec, state):
    """Test that a signed builder-credentialed deposit is dropped."""
    deposit_request = prepare_process_deposit_request(spec, state, for_builder=True, signed=True)
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=True,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__builder_credentials_dropped_unsigned(spec, state):
    """Test that an unsigned builder-credentialed deposit is dropped."""
    deposit_request = prepare_process_deposit_request(spec, state, for_builder=True, signed=False)
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=True,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__builder_credentials_dropped_existing_builder(spec, state):
    """Test that a builder-credentialed deposit for an existing builder pubkey is dropped.

    Top-ups, like first deposits, must use the builder deposit request.
    """
    assert len(state.builders) > 0
    deposit_request = prepare_process_deposit_request(
        spec, state, builder_index=0, signed=True, amount=spec.MIN_DEPOSIT_AMOUNT
    )
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=True,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__builder_credentials_dropped_nonstandard_padding(spec, state):
    """Test that the drop rule keys on the prefix alone, regardless of padding."""
    withdrawal_credentials = spec.BUILDER_WITHDRAWAL_PREFIX + b"\xff" * 11 + b"\x59" * 20
    deposit_request = prepare_process_deposit_request(
        spec,
        state,
        for_builder=True,
        signed=True,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=True,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__routing__validator_pubkey_builder_credentials(spec, state):
    """Test that an existing validator pubkey with builder credentials is dropped.

    The prefix check precedes any pubkey lookup.
    """
    withdrawal_credentials = spec.BUILDER_WITHDRAWAL_PREFIX + b"\x00" * 11 + b"\x59" * 20
    deposit_request = prepare_process_deposit_request(
        spec,
        state,
        validator_index=0,
        signed=True,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=True,
    )


#
# Validator deposits are unaffected
#


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__routing__builder_pubkey_validator_credentials(spec, state):
    """Test that an existing builder pubkey with validator credentials joins the queue.

    The same public key may be registered as both a validator and a builder
    (EIP-8282): a non-builder-credentialed deposit is a validator deposit, even
    when its pubkey is an existing builder's.
    """
    builder_pubkey = state.builders[0].pubkey
    amount = spec.MIN_ACTIVATION_BALANCE
    withdrawal_credentials = spec.BLS_WITHDRAWAL_PREFIX + spec.hash(builder_pubkey)[1:]

    deposit_request = prepare_process_deposit_request(
        spec,
        state,
        builder_index=0,
        amount=amount,
        signed=True,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=False,
        expected_pending_deposit_pubkey=builder_pubkey,
        expected_pending_deposit_amount=amount,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__routing__validator_pubkey_validator_credentials(spec, state):
    """Test that an existing validator pubkey with validator credentials joins the queue."""
    validator_pubkey = state.validators[0].pubkey
    amount = spec.MIN_DEPOSIT_AMOUNT

    deposit_request = prepare_process_deposit_request(
        spec,
        state,
        validator_index=0,
        amount=amount,
        signed=True,
    )
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=False,
        expected_pending_deposit_pubkey=validator_pubkey,
        expected_pending_deposit_amount=amount,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__routing__new_pubkey_validator_credentials(spec, state):
    """Test that a new pubkey with validator credentials joins the queue."""
    new_validator_index = len(state.validators)
    amount = spec.MIN_ACTIVATION_BALANCE

    deposit_request = prepare_process_deposit_request(spec, state, signed=True, amount=amount)
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=False,
        expected_pending_deposit_pubkey=pubkeys[new_validator_index],
        expected_pending_deposit_amount=amount,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__routing__new_pubkey_eth1_credentials(spec, state):
    """Test that a new pubkey with eth1 (0x01) credentials joins the queue."""
    withdrawal_credentials = spec.ETH1_ADDRESS_WITHDRAWAL_PREFIX + b"\x00" * 11 + b"\x42" * 20
    amount = spec.MIN_ACTIVATION_BALANCE

    deposit_request = prepare_process_deposit_request(
        spec,
        state,
        signed=True,
        amount=amount,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=False,
        expected_pending_deposit_amount=amount,
        expected_pending_deposit_credentials=withdrawal_credentials,
    )


@with_gloas_and_later
@spec_state_test
def test_process_deposit_request__routing__new_pubkey_compounding_credentials(spec, state):
    """Test that a new pubkey with compounding (0x02) credentials joins the queue."""
    withdrawal_credentials = spec.COMPOUNDING_WITHDRAWAL_PREFIX + b"\x00" * 11 + b"\x42" * 20
    amount = spec.MAX_EFFECTIVE_BALANCE_ELECTRA

    deposit_request = prepare_process_deposit_request(
        spec,
        state,
        signed=True,
        amount=amount,
        withdrawal_credentials=withdrawal_credentials,
    )
    pre_state = state.copy()

    yield from run_deposit_request_processing(spec, state, deposit_request)

    assert_process_deposit_request(
        spec,
        state,
        pre_state,
        deposit_request=deposit_request,
        is_dropped=False,
        expected_pending_deposit_amount=amount,
        expected_pending_deposit_credentials=withdrawal_credentials,
    )
