class ZCL_FERROVIARIO definition
  public
  create public .

public section.

  class-methods GET_NEW_ID_REFKEY
    returning
      value(R_ID_ZLEST0019) type ZID_ZLEST0019
    raising
      ZCX_FERROVIARIO .
  methods GET_ID_REFKEY
    returning
      value(R_ID_REFKEY) type ZID_ZLEST0019 .
  methods SET_ID_REFKEY
    importing
      !I_ID_REFKEY type ZID_ZLEST0019 .
protected section.

  data ID_REFKEY type ZID_ZLEST0019 .
private section.
ENDCLASS.



CLASS ZCL_FERROVIARIO IMPLEMENTATION.


  method GET_ID_REFKEY.
    R_ID_REFKEY = ME->ID_REFKEY.
  endmethod.


  METHOD GET_NEW_ID_REFKEY.

    CLEAR: R_ID_ZLEST0019.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZSEQ_LES19'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FERROVIARIO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '1'
        OBJECT                  = 'ZSEQ_LES19'
        QUANTITY                = '00000000000000000001'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = R_ID_ZLEST0019
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FERROVIARIO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        OBJECT           = 'ZSEQ_LES19'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FERROVIARIO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD SET_ID_REFKEY.
    ME->ID_REFKEY = I_ID_REFKEY.
  ENDMETHOD.
ENDCLASS.
