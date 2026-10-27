# Managed lazy sequences

In `:runtime-mode :forkable`, SCI owns the realization state of `lazy-seq`
and the supported core sequence producers. Standard mode continues to use
native sequences. Forking snapshots deferred computation; it does not execute
that computation or make its external effects reversible.

## Observable contract

1. Creating or forking a pending sequence does not call its body.
2. A fork inherits the prefix already realized in its source world. Pending
   work and subsequent cache changes belong to the consuming world.
3. Aliases, interpreted closure captures, and persistent containers refer to
   the same logical sequence. They select the current related world rather
   than sharing one native realization cache across worlds.
4. Host consumption of a returned sequence selects its returning context.
   Explicit evaluation in a related context takes precedence; an unrelated
   context cannot retarget it.
5. Thunk invocation failures remain retryable. Nested normalization failures
   follow the respective JVM/JavaScript host behavior, including the newer
   JVM pending marker that remains unset until normalization succeeds. A
   small native behavior probe selects this contract. Chunked producers keep
   their host's chunk realization behavior; forcing one item can therefore
   evaluate a whole chunk.
6. JVM realization in one world is serialized per sequence cache. Forking
   waits for active evaluation and snapshots at a quiescent boundary.
   Fork/retirement reentry from that world's own evaluation or copy is refused.

These are value and isolation guarantees, not guarantees that a sequence
completes, is deterministic, or has no effects. In particular, hashing an
infinite sequence still does not terminate. The copier identifies handles by
identity and never hashes their sequence contents as bookkeeping.

Standard JVM lazy bodies also use reusable interpreter thunks. The host
`lazy-seq` macro's `:once` closure optimization can clear captured interpreter
machinery on a failed call; it must not prevent SCI's native lazy value from
retrying its body on the next consumption.

Trusted built-in macro functions in forkable contexts materialize generated
code into eager immutable syntax. This includes direct calls through resolved
macro Vars, as used by interpreted CPS transformations. Collection shape,
metadata and unchanged immutable subtrees are preserved; quoted payloads and
opaque host objects are left alone. This adapts compiler syntax, not arbitrary
runtime lazy values. Standard native macro functions and interpreted user
macros keep their existing behavior.

## Representation and lifetime

A lazy value has a stable logical owner. Each world has an independent state
box for that owner: pending thunk, returned intermediate value, or normalized
sequence. Copying a box copies immutable state references without running the
thunk. SCI reference operations inside interpreted closures select the active
world as usual.

Lazy owners do not allocate a permanent dense world slot for every element.
The lineage indexes them weakly; owners index their per-world boxes by weak
world keys. Unreachable prefixes, owners, and their payloads can be collected.
This differs deliberately from installing a dense managed reference per node,
which would retain growing descriptor metadata for long-lived streams.
This is not a redesign of namespace ownership: shared lineage descriptors and
native primary Var roots can still retain ancestor state used by a live child.
Retirement releases the selected world's managed cells and cache entries; it
does not promise reclamation of all shared namespace anchors.

Weak keys alone do not solve every lifetime: a cache value can capture its own
world. A logical owner retained by an ancestor can also retain an active
descendant's realized prefix until that descendant retires; the weak index is
not a per-world tracing collector. Embedders must call `sci/retire!` when a
forkable context is permanently discarded. Retirement removes that world's
boxes, clears its dense cells, and
refuses later evaluation, mutation, or forking. It leaves related worlds usable
and does not close arbitrary host resources. A returned view whose home has
retired cannot use that home as a fallback.

Retirement is idempotent. If a cooperating cache owner fails cleanup, the
world remains closed and the retained context can retry retirement. If a fork
fails and retiring its unpublished target also fails, the thrown exception
retains both failures and a recovery capability. Keep it and use
`sci/recover-fork!`; this retries target cleanup, without repeating the fork or
consuming its source. Do not drop that recovery exception before cleanup.

## Supported and explicit boundaries

The managed core overrides cover the ordinary map/filter/keep/concat family,
take/drop families, partitions and reductions, repetition and iteration,
tree/flatten producers, sorted subsequences, and their supported transducer
arities. `sci.impl.lazy-producers/producer-functions` is the exact inventory.
Stateful transducer arities use world-local SCI reference state. The optional
`sci.interrupt` producer overrides dispatch to these implementations in
forkable contexts and consult the consuming context's interrupt callback.

The transducing arities of `sequence` and `eduction` reject creation explicitly:
their native cursor state does not yet have a managed representation. Regex
matcher streams, iterators, reader streams, foreign native lazy values, and
host libraries that internally create such values require explicit host
cooperation. Direct affine values in managed world cells are rejected by
`Forkable` policy; supplying one directly to a managed producer is rejected
rather than concealed inside its closure. Opaque host closures and nested
foreign graphs are not reflectively traversed. Raw host capabilities supplied
through `:bindings` are not implicitly converted into managed state.
An adapter captured inside a producer must select its own state by the active
world. Merely implementing a clone-on-fork policy for a cursor does not rewrite
references hidden in an already captured host closure.

Native arithmetic range state is used internally as trusted immutable input,
with managed realization around it. Its value behavior is preserved, but
counting the managed result traverses it rather than using the native range's
constant-time count. No standard-mode producer override is installed.

The implementation targets JVM and JavaScript runtimes with weak-reference
support. Managed laziness on ClojureDart is explicitly unsupported; standard
mode is unchanged. Tests must distinguish this implementation scope from
foreign-resource cooperation or execution snapshots inside a running thunk.
