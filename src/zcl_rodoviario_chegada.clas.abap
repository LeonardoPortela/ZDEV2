class ZCL_RODOVIARIO_CHEGADA definition
  public
  final
  create public .

public section.

  interfaces ZIF_CADASTRO .

  methods GET_NEW_ID_REFKEY
    returning
      value(R_ID_ZLEST0019) type ZID_ZLEST0019
    raising
      ZCX_RODOVIARIO .
  methods MONTA_PROCESSO
    importing
      !I_NOTAS type ZDE_ZLEST0019_L1_30_T .
  methods PROCESSAR_DADOS
    exporting
      !E_LOGS type ZLEST0176_T .
protected section.
private section.

  data PESO_NOTAS_CHEGADA type ZDE_ZLEST0019_L1_30_T .
ENDCLASS.



CLASS ZCL_RODOVIARIO_CHEGADA IMPLEMENTATION.


  method GET_NEW_ID_REFKEY.

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
      RAISE EXCEPTION TYPE ZCX_RODOVIARIO
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
      RAISE EXCEPTION TYPE ZCX_RODOVIARIO
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
      RAISE EXCEPTION TYPE ZCX_RODOVIARIO
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


  endmethod.


  method MONTA_PROCESSO.

    ME->PESO_NOTAS_CHEGADA = I_NOTAS.

  endmethod.


  method PROCESSAR_DADOS.

    DATA: WL_ZLEST0176 TYPE ZLEST0176,
          IT_ZLEST0176 TYPE ZLEST0176_T,
          IT_LOGS      TYPE ZLEST0008_T,
          V_CONT       TYPE ZLEST0176-CONT,
          V_CONT_ITEM  TYPE ZLEST0176-CONT_ITEM.


    CLEAR: E_LOGS[], IT_LOGS[], IT_ZLEST0176[], V_CONT, V_CONT_ITEM.

    PERFORM F_PROC_REGISTROS_EXTERNO IN PROGRAM ZLESI0005
                                          USING ME->PESO_NOTAS_CHEGADA
                                       CHANGING IT_LOGS.


*---------------------------------------------------------------------*
*   Gravar Logs
*---------------------------------------------------------------------*
    SELECT MAX( CONT )
      FROM ZLEST0176 INTO V_CONT.

    ADD 1 TO V_CONT.

    LOOP AT IT_LOGS INTO DATA(_WL_LOG).
      CLEAR: WL_ZLEST0176.

      ADD 1 TO V_CONT_ITEM.

      WL_ZLEST0176-CONT         = V_CONT.
      WL_ZLEST0176-CONT_ITEM    = V_CONT_ITEM.
      WL_ZLEST0176-MSGTYP       = _WL_LOG-MSGTYP.
      WL_ZLEST0176-MSGNR        = _WL_LOG-MSGNR.
      WL_ZLEST0176-MSGV1        = _WL_LOG-MSGV1.
      WL_ZLEST0176-DT_REGISTRO  = _WL_LOG-DATA.
      WL_ZLEST0176-HR_REGISTRO  = _WL_LOG-HORA.
      WL_ZLEST0176-US_REGISTRO  = _WL_LOG-USUARIO.

      MODIFY ZLEST0176 FROM WL_ZLEST0176.
      APPEND WL_ZLEST0176 TO IT_ZLEST0176.
    ENDLOOP.

    E_LOGS[] = IT_ZLEST0176[].

  endmethod.
ENDCLASS.
