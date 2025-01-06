---
title: GarbageCollection
---

Utilities for interacting with grains garbage collector.

```grain
GrabageCollection
```

## Values

Functions and constants included in the GarbageCollection module.

### GarbageCollection.**setFinalizer**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
setFinalizer : (finalizer: (a => Void), value: a) => Option<Exception>
```

Set a finalizer function for a value.
The finalizer function will be called when the value is garbage collected.

Parameters:

|param|type|description|
|-----|----|-----------|
|`finalizer`|`a => Void`|The finalizer function|
|`value`|`a`|The value to set the finalizer for|

Returns:

|type|description|
|----|-----------|
|`Option<Exception>`|`None` if the finalizer was set successfully or `Some(NonGCType)` if the value is not a GC type.|

