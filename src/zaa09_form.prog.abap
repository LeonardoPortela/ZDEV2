*----------------------------------------------------------------------*
***INCLUDE ZAA09_FORM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHAMA_BABI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHAMA_BABI .

  DATA: LS_MSSG_RETURN       TYPE BAPIRET2,
        LS_POST_INFORMATION  TYPE BAPI1022_FEGLG002,
        LS_POST_INFORMATIONX TYPE BAPI1022_FEGLG002X.

  DATA: VL_MSG_EXIBIR TYPE STRING.

  LOOP AT TG_ANLA.

    IF TG_ANLA-DEAKT IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    LS_POST_INFORMATION-DEACT_DATE  = S_DEAKT-LOW.
    LS_POST_INFORMATIONX-DEACT_DATE = 'X'.

    CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
      EXPORTING
        COMPANYCODE          = TG_ANLA-BUKRS
        ASSET                = TG_ANLA-ANLN1
        SUBNUMBER            = TG_ANLA-ANLN2
        POSTINGINFORMATION   = LS_POST_INFORMATION
        POSTINGINFORMATIONX  = LS_POST_INFORMATIONX
      IMPORTING
        RETURN               = LS_MSSG_RETURN.

    IF LS_MSSG_RETURN-TYPE EQ 'E'.
      CLEAR: VL_MSG_EXIBIR.
      CONCATENATE 'Erro na desativação do Imobilizado:' TG_ANLA-ANLN1 '- SubNº:' TG_ANLA-ANLN2
                  '(' LS_MSSG_RETURN-MESSAGE ')!'
             INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

      WRITE: / VL_MSG_EXIBIR.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CLEAR: VL_MSG_EXIBIR.
      CONCATENATE 'Imobilizado:' TG_ANLA-ANLN1 '- SubNº:' TG_ANLA-ANLN2
                  'desativado!'
             INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

      WRITE: / VL_MSG_EXIBIR.

    ENDIF.

    CLEAR: LS_POST_INFORMATION, LS_POST_INFORMATIONX.

  ENDLOOP.

  "MESSAGE 'Imobilizados Desativados' TYPE 'S'.


ENDFORM.
