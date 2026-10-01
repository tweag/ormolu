module RioDeepseqExample where

import RIO

result =
  forceEvaluationOfBigStructure `deepseq`
    continueWithNextStep
