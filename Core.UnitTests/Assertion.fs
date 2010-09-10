#light
namespace Tim.Lisp.Core.UnitTests
open Xunit

module Assertion =
    let shouldEqual<'T> (expected : 'T) (actual : 'T) =
        Assert.Equal(expected, actual)
