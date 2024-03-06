---
title: Process
---

Utilities for accessing functionality and information about the Grain program's process.

This includes things like accessing environment variables and sending signals.

```grain
from "wasi/process" include Process
```

## Types

Type declarations included in the Process module.

### Process.**Signal**

```grain
enum Signal {
  HUP,
  INT,
  QUIT,
  ILL,
  TRAP,
  ABRT,
  BUS,
  FPE,
  KILL,
  USR1,
  SEGV,
  USR2,
  PIPE,
  ALRM,
  TERM,
  CHLD,
  CONT,
  STOP,
  TSTP,
  TTIN,
  TTOU,
  URG,
  XCPU,
  XFSZ,
  VTALRM,
  PROF,
  WINCH,
  POLL,
  PWR,
  SYS,
}
```

Signals that can be sent to the host system.

Variants:

#### HUP

```grain
HUP
```

Hangup.

#### INT

```grain
INT
```

Terminate interrupt signal.

#### QUIT

```grain
QUIT
```

Terminal quit signal.

#### ILL

```grain
ILL
```

Illegal instruction.

#### TRAP

```grain
TRAP
```

Trace/breakpoint trap.

#### ABRT

```grain
ABRT
```

Process abort signal.

#### BUS

```grain
BUS
```

Access to an undefined portion of a memory object.

#### FPE

```grain
FPE
```

Erroneous arithmetic operation.

#### KILL

```grain
KILL
```

Kill.

#### USR1

```grain
USR1
```

User-defined signal 1.

#### SEGV

```grain
SEGV
```

Invalid memory reference.

#### USR2

```grain
USR2
```

User-defined signal 2.

#### PIPE

```grain
PIPE
```

Write on a pipe with no one to read it.

#### ALRM

```grain
ALRM
```

Alarm clock.

#### TERM

```grain
TERM
```

Termination signal.

#### CHLD

```grain
CHLD
```

Child process terminated, stopped, or continued.

#### CONT

```grain
CONT
```

Continue executing, if stopped.

#### STOP

```grain
STOP
```

Stop executing.

#### TSTP

```grain
TSTP
```

Terminal stop signal.

#### TTIN

```grain
TTIN
```

Background process attempting read.

#### TTOU

```grain
TTOU
```

Background process attempting write.

#### URG

```grain
URG
```

High bandwidth data is available at a socket.

#### XCPU

```grain
XCPU
```

CPU time limit exceeded.

#### XFSZ

```grain
XFSZ
```

File size limit exceeded.

#### VTALRM

```grain
VTALRM
```

Virtual timer expired.

#### SYS

```grain
SYS
```

Bad system call.

## Values

Functions and constants included in the Process module.

### Process.**argv**

```grain
argv : () => Result<Array<String>, Exception>
```

Access command line arguments.

Returns:

|type|description|
|----|-----------|
|`Result<Array<String>, Exception>`|`Ok(args)` of an array containing positional string arguments to the process if successful or `Err(exception)` otherwise|

### Process.**env**

```grain
env : () => Result<Array<String>, Exception>
```

Access environment variables.

Returns:

|type|description|
|----|-----------|
|`Result<Array<String>, Exception>`|`Ok(vars)` of an array containing environment variables supplied to the process if successful or `Err(exception)` otherwise|

### Process.**exit**

```grain
exit : (code: Number) => Result<Void, Exception>
```

Terminate the process normally.

Parameters:

|param|type|description|
|-----|----|-----------|
|`code`|`Number`|The value to exit with. An exit code of 0 is considered normal, with other values having meaning depending on the platform|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Err(exception)` if unsuccessful. Will not actually return a value if successful, as the process has ended|

### Process.**sigRaise**

```grain
sigRaise : (signal: Signal) => Result<Void, Exception>
```

Send a signal to the process of the calling thread.

Parameters:

|param|type|description|
|-----|----|-----------|
|`signal`|`Signal`|The signal to send|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### Process.**schedYield**

```grain
schedYield : () => Result<Void, Exception>
```

Yield execution to the calling thread.

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

