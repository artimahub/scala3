# Doc review digest: library/src/scala/concurrent/BatchingExecutor.scala

- models: writer devstral-2512 | accuracy mistral-medium-2508 | style mistral-large-2512 | adjudicator devstral-2512
- converged: false (up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- accuracy verdict: approve
- style verdict: revise
- ADJUDICATOR verdict (final): revise

## Reviewer disagreements the adjudicator settled

- L40 `BatchingExecutorStatics.MissingParentBlockContext.blockOn` -> ruled for **style**
  - accuracy: 
  - style: The description states the method throws an `IllegalStateException`, but the implementation also executes the `thunk` before throwing. The return description is also incorrect because the method never returns the result of the `thunk` (it always throws).
  - why: The style reviewer's suggestion clarifies the behavior more accurately.
- L138 `AbstractBatch.push` -> ruled for **style**
  - accuracy: 
  - style: The description does not mention that the method may resize the `other` array if it is full. This is a material behavior that callers should know.
  - why: The style reviewer's suggestion adds important information about resizing.
- L145 `AbstractBatch.runN` -> ruled for **style**
  - accuracy: 
  - style: The description does not mention that the method may throw an `InterruptedException` if the thread is interrupted during execution. This is a material edge case.
  - why: The style reviewer's suggestion adds important information about exceptions.
- L187 `AsyncBatch.run` -> ruled for **style**
  - accuracy: 
  - style: The description does not mention that this method sets the `_tasksLocal` thread-local variable, which is a material side effect. It also does not mention that it may throw exceptions from the executed tasks.
  - why: The style reviewer's suggestion adds important information about side effects and exceptions.
- L198 `AsyncBatch.apply` -> ruled for **style**
  - accuracy: 
  - style: The description does not mention that the method may return a `Throwable` if one occurs during execution, nor does it clarify that the `BlockContext` is used for blocking operations within the batch.
  - why: The style reviewer's suggestion adds important information about return values and context usage.
- L240 `AsyncBatch.blockOn` -> ruled for **style**
  - accuracy: 
  - style: The description does not mention that the method may execute the batch immediately if tasks are queued, which is a critical behavior for avoiding deadlocks. It also does not clarify that the `thunk` is executed within the parent `BlockContext`.
  - why: The style reviewer's suggestion adds important information about execution behavior.
- L256 `SyncBatch.run` -> ruled for **style**
  - accuracy: 
  - style: The description does not mention that the method may throw an `InterruptedException` if the thread is interrupted during execution, which is a material edge case.
  - why: The style reviewer's suggestion adds important information about exceptions.

## Outstanding worklist at the end

- L40 `BatchingExecutorStatics.MissingParentBlockContext.blockOn` [blocker/style]: Change to: "Executes the given `thunk` and then throws an `IllegalStateException` indicating a bug when `parentBlockContext` is null."
- L138 `AbstractBatch.push` [blocker/style]: Change to: "Adds a `Runnable` to the batch. If the `other` array is full, it is resized to accommodate additional tasks."
- L145 `AbstractBatch.runN` [blocker/style]: Change to: "Executes up to `n` `Runnable` tasks from the batch. If the thread is interrupted during execution, an `InterruptedException` may be thrown."
- L187 `AsyncBatch.run` [blocker/style]: Change to: "Executes the batch of `Runnable` tasks. Sets the thread-local `_tasksLocal` to this batch during execution. Exceptions thrown by the tasks are propagated to the caller."
- L198 `AsyncBatch.apply` [blocker/style]: Change to: "Executes the batch of `Runnable` tasks within the given `BlockContext`. Any `Throwable` thrown during execution is returned; otherwise, `null` is returned."
- L240 `AsyncBatch.blockOn` [blocker/style]: Change to: "Executes a blocking operation, delegating to the parent `BlockContext`. If tasks are queued in the batch, they are executed immediately to avoid deadlocks."
- L256 `SyncBatch.run` [blocker/style]: Change to: "Executes the batch of `Runnable` tasks synchronously. If the thread is interrupted during execution, an `InterruptedException` may be thrown."
- L27 `BatchingExecutorStatics.emptyBatchArray` [nit/style]: Change to: "A shared empty array of `Runnable | Null` used to avoid allocations in batches with no additional tasks."
- L31 `BatchingExecutorStatics.syncPreBatchDepth` [nit/style]: Change to: "The maximum depth of nested `Runnable` executions before switching to batching mode to prevent stack overflow."
- L35 `BatchingExecutorStatics.runLimit` [nit/style]: Change to: "The maximum number of `Runnable` tasks processed in a single batch to prevent starvation of other tasks on the thread pool."
- L117 `AbstractBatch.first` [nit/style]: Change to: "The first `Runnable` in the batch, stored unboxed for efficiency. May be `null` if the batch is empty."
- L119 `AbstractBatch.other` [nit/style]: Change to: "An array storing additional `Runnable` tasks beyond the first one. May contain `null` values and is reused for efficiency."
- L121 `AbstractBatch.size` [nit/style]: Change to: "The current number of `Runnable` tasks in the batch, including both the `first` field and any tasks in the `other` array."
- L182 `AsyncBatch.this` [nit/style]: Change to: "Creates a new `AsyncBatch` initialized with the given `Runnable` as its first task."

## Inline NEEDS-HUMAN markers left in source
(none)
