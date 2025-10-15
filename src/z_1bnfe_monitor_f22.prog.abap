*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F22
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  check_connection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_CONNECTION .

  DATA: LV_BUKRS TYPE BUKRS,
        LV_BUPLA TYPE J_1BBRANC_,
        LV_MODEL TYPE J_1BMODEL.                            "1155424

* Get company code and business place from selection parameters
  IF NOT BUKRS IS INITIAL AND
     NOT BUPLA IS INITIAL AND
     NOT MODEL IS INITIAL.                                  "1155424
    LV_BUKRS = BUKRS-LOW.
    LV_BUPLA = BUPLA-LOW.
    LV_MODEL = MODEL-LOW.                                   "1155424
  ELSE.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '054'.
  ENDIF.

* Check RFC connection
  CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
    EXPORTING
      I_BUKRS   = LV_BUKRS
      I_BRANCH  = LV_BUPLA
      I_MODEL   = LV_MODEL                                  "1155424
    EXCEPTIONS
      RFC_ERROR = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID 'J1B_NFE' TYPE 'I' NUMBER '052'.
  ELSE.
    MESSAGE ID 'J1B_NFE' TYPE 'I' NUMBER '053'.
  ENDIF.

ENDFORM.                    " check_connection

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_DANFE_DACTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIME_DANFE_DACTE .

* Check if an NF-e selection was made
  IF IT_SELECTED_ROWS IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.

    READ TABLE IT_NFE_ALV INTO WA_NFE_ALV INDEX WA_SELECTED_ROWS-INDEX.
* process numbering only for RFC call types 1 and 2

    CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
      EXPORTING
        DOC_NUMERO     = WA_NFE_ALV-DOCNUM
      EXCEPTIONS
        NAO_LOCALIZADO = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    "Caso tenha um pedágio REPOM será impresso
    IF WA_NFE_ALV-MODEL EQ '57'.

      CALL METHOD ZCL_REPOM_VIAGEM_VPR=>IMPRIMIR_VIAGEM
        EXPORTING
          I_DOCNUM_CTE = WA_NFE_ALV-DOCNUM
        EXCEPTIONS
          ERRO         = 1
          OTHERS       = 2.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " IMPRIME_DANFE_DACTE
