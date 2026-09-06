module RioExample where

import RIO

message =
  greetingText <> userNameText
    & Text.strip
