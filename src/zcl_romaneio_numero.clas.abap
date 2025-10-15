class ZCL_ROMANEIO_NUMERO definition
  public
  final
  create public .

public section.

  class-methods GET_NEXT_NUMBER
    importing
      !I_BRANCH type J_1BBRANC_
      !I_SAFRA type ZNR_SAFRA
      !I_INTERFACE type ZID_INTERF
    returning
      value(R_NUMERO) type ZNR_ROMANEIO
    raising
      ZCX_ROMANEIO_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ROMANEIO_NUMERO IMPLEMENTATION.


  METHOD GET_NEXT_NUMBER.

    DATA: CHAVE  TYPE CHAR8.

    CLEAR: R_NUMERO.

    CONCATENATE I_BRANCH I_SAFRA INTO CHAVE.

    CALL FUNCTION 'ZENQUEUE_NUM_ROMANEIO'
      EXPORTING
        CHAVE          = CHAVE
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ROMANEIO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = SY-MSGTY
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ELSE.

      CASE I_INTERFACE.
        WHEN ZCL_ROMANEIO=>INTERFACE_CARGA_SAP.

          SELECT SINGLE *
            FROM ZSDT0154
            INTO @DATA(WA_ZSDT0154)
           WHERE NR_SAFRA  EQ @I_SAFRA
             AND ID_BRANCH EQ @I_BRANCH.

          IF SY-SUBRC IS INITIAL.
            ADD 1 TO WA_ZSDT0154-SEQ_ATUAL.
            R_NUMERO = WA_ZSDT0154-SEQ_ATUAL.
          ELSE.
            R_NUMERO = 700000.
            WA_ZSDT0154-NR_SAFRA = I_SAFRA.
            WA_ZSDT0154-ID_BRANCH = I_BRANCH.
            WA_ZSDT0154-SEQ_ATUAL = 700000.
          ENDIF.
          MODIFY ZSDT0154 FROM WA_ZSDT0154.

        WHEN OTHERS.

          SELECT SINGLE *
            FROM ZSDT0136
            INTO @DATA(WA_ZSDT0136)
           WHERE SAFRA EQ @I_SAFRA
             AND WERKS EQ @I_BRANCH.

          IF SY-SUBRC IS INITIAL.
            ADD 1 TO WA_ZSDT0136-SEQ_ATUAL.
            R_NUMERO = WA_ZSDT0136-SEQ_ATUAL.
          ELSE.
            R_NUMERO = 1.
            WA_ZSDT0136-SAFRA = I_SAFRA.
            WA_ZSDT0136-WERKS = I_BRANCH.
            WA_ZSDT0136-SEQ_ATUAL = 1.
          ENDIF.
          MODIFY ZSDT0136 FROM WA_ZSDT0136.

      ENDCASE.

      COMMIT WORK.

      CALL FUNCTION 'ZDENQUEUE_NUM_ROMANEIO'
        EXPORTING
          CHAVE = CHAVE.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
