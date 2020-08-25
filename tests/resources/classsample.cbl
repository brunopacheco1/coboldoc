      $if jvmgen set
        $set ilnamespace"com.coboldoc.mexception"
      $else
        $set ilnamespace"coboldoc.mexception"
      $end

      *>> <summary>
      *>> SuccessException - Exception thrown if the test case ends with a sucess condition
      *>> </summary>
       class-id SuccessException  public inherits type Exception
      $if nojvmgen set
           attribute Serializable
      $end
           .

           *>> <summary>
           *>> SuccessException - constructor with no assert message
           *>> </summary>
           method-id New.
           procedure division.
           end method.

           *>> <summary>
           *>> SuccessException - constructor with assert message
           *>> </summary>
           *>> <param name="msg">message</param>
           method-id New.
           procedure division using by value msg as string.
           end method.
       end class.

       *>> <summary>
       *>> AssertionFailedException - Thrown when an assertion failed.
       *>> </summary>
       class-id AssertionFailedException  public inherits type Exception
      $if nojvmgen set
           attribute Serializable
      $end
           .
           *>> <summary>
           *>> AssertMessage - Property for assert message
           *>> </summary>
           01 AssertMessage              string public property.

           *>> <summary>
           *>> AssertionFailedException - constructor with no assert message
           *>> </summary>
           method-id New.
           procedure division.
           end method.


           *>> <summary>
           *>> AssertionFailedException - constructor with assert message
           *>> </summary>
           *>> <param name="msg">message</param>
           method-id New.
           procedure division using  by value msg as string.
           end method.
       end class.

       *>> <summary>
       *>> InconclusiveException - Thrown when an Inconclusive failure assert is used.
       *>> </summary>
       class-id InconclusiveException public inherits type Exception
           .
           *>> <summary>
           *>> AssertMessage - Property for assert message
           *>> </summary>
           01 AssertMessage              string public property.


           *>> <summary>
           *>> InconclusiveException - constructor with assert message
           *>> </summary>
           *>> <param name="msg">message</param>
           method-id New.
           procedure division.
           end method.

           *>> <summary>
           *>> InconclusiveException - constructor with assert message
           *>> </summary>
           *>> <param name="msg">message</param>
           method-id New.
           procedure division using  by value msg as string.
           end method.
       end class.

       *>> <summary>
       *>> AssertErrorException - Thrown when an assertion failed.
       *>> </summary>
       class-id AssertErrorException  public inherits type Exception
      $if nojvmgen set
           attribute Serializable
      $end
           .
           *>> <summary>
           *>> AssertMessage - Property for assert message
           *>> </summary>
           01 AssertMessage              string public property.

           *>> <summary>
           *>> AssertMessage - Property for assert message
           *>> </summary>
           01 CauseOfException             type Exception public property.

           *>> <summary>
           *>> AssertErrorException - constructor with no assert message
           *>> </summary>
           method-id New.
           procedure division.
           end method.


           *>> <summary>
           *>> AssertErrorException - constructor with assert message
           *>> </summary>
           *>> <param name="msg">message</param>
           method-id New.
           procedure division using  by value msg as string.
           end method.

           *>> <summary>
           *>> AssertErrorException - constructor with assert message
           *>> </summary>
           *>> <param name="msg">message</param>
           *>> <param name="cause">inner exception</param>
           method-id New.
           procedure division using by value msg as string
                                    by value cause as type Exception.
           end method.
       end class.