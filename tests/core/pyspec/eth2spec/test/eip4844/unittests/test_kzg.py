
from eth2spec.test.helpers.constants import (
    EIP4844,
    MINIMAL,
)
from eth2spec.test.helpers.sharding import (
    create_blob,
)
from eth2spec.test.context import (
    with_phases,
    spec_state_test,
    with_presets,
)


@with_phases([EIP4844])
@spec_state_test
@with_presets([MINIMAL])
def test_blob_to_kzg(spec, state):
    blob = create_blob(spec)
    spec.blob_to_kzg(blob)
