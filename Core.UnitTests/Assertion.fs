#light
namespace Tim.Lisp.Core.UnitTests
open NUnit.Framework

module Assertion =
    let shouldEqual<'T> (expected : 'T) (actual : 'T) =
        Assert.AreEqual(box expected, box actual)
