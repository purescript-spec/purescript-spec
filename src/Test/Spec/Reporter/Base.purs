module Test.Spec.Reporter.Base (
  update,
  summarize,
  reporter,
  BaseReporter
  ) where

import Prelude
import Test.Spec              (Group(), Result())
import Test.Spec.Runner.Event (Event)

type Update s m r = s -> Event -> m r
type Summarize s m = s -> Array (Group Result) -> m Unit

newtype BaseReporter s m = BaseReporter {
  state     :: s
, update    :: Update s m (BaseReporter s m)
, summarize :: Summarize s m
}

update :: ∀ s m. Event -> BaseReporter s m -> m (BaseReporter s m)
update e (BaseReporter { state: s, update: f }) = f s e

summarize :: ∀ s m. Array (Group Result) -> BaseReporter s m -> m Unit
summarize xs (BaseReporter { state, summarize: f }) = f state xs

reporter
  :: ∀ s m
   . (Monad m)
  => s
  -> Update s m s
  -> Summarize s m
  -> BaseReporter s m
reporter state update summarize = BaseReporter { state, update: go, summarize }
  where
  go s e = do
    s' <- update s e
    pure $ BaseReporter { state: s', update: go, summarize }
