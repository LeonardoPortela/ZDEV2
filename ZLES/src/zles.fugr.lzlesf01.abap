*----------------------------------------------------------------------*
***INCLUDE LZLESF01 .
*----------------------------------------------------------------------*

FORM Z_GRAVA_LEGADO.

  DATA: IT_ZLEST0002 TYPE ZLEST0002 OCCURS 0 WITH HEADER LINE.

  MOVE-CORRESPONDING ZLEST0002 TO IT_ZLEST0002.
  APPEND IT_ZLEST0002.

*--> 25.08.2023 16:19:00 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_LES_OUTBOUND_VEICULO' IN BACKGROUND TASK
*    DESTINATION 'XI_VEICULO'
*    AS SEPARATE UNIT
*    TABLES
*      IT_VEICULO = IT_ZLEST0002.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_LES_OUTBOUND_VEICULO'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        it_veiculo = IT_ZLEST0002.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        it_veiculo = IT_ZLEST0002.
  ENDIF.
*<-- 25.08.2023 16:19:00 - Migração S4 – ML – Fim

  COMMIT WORK.

ENDFORM.                    "Z_GRAVA_LEGADO

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.
    WHEN 'BT_OK'.
*     Leva Valores
      PERFORM Z_LEVA_VALORES.
    WHEN OTHERS.

      IF NOT V_PLACA1 IS INITIAL AND NOT V_PLACA2 IS INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&      Form  z_dt_hr_usr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM Z_DT_HR_USR.
  ZLEST0002-ERDAT = SY-DATUM.
  ZLEST0002-ERZET = SY-UZEIT.
  ZLEST0002-ERNAM = SY-UNAME.
ENDFORM.                    "z_dt_hr_usr

*&---------------------------------------------------------------------*
*&      Form  Z_LEVA_VALORES                                           *
*&---------------------------------------------------------------------*
*                                Leva Valores                          *
*----------------------------------------------------------------------*
FORM Z_LEVA_VALORES.

  DATA: V_COUNTRY01      TYPE J_1BTREG_CITY-COUNTRY,
        V_CIDADE01       TYPE J_1BTREG_CITY-TAXJURCODE,
        V_PROPRIETARIO01 TYPE LIFNR.

  DATA: WA_VT_L TYPE VTTKVB.

  READ TABLE TI_XVTTK INTO WA_VT_L INDEX 1.

*  if ( v_placa1 is initial ) or ( v_placa2 is initial ).
*    message i836(sd) with 'Deve ser preenchido as Placas (Cavalo e Carreta)!'.
*    exit.
*  endif.

  IF ( V_PLACA1 IS INITIAL ).
    MESSAGE I836(SD) WITH 'Deve ser preenchido a Placa do Cavalo!'.
    EXIT.
  ENDIF.

  IF ( V_PLACA2 IS INITIAL ) AND ( NOT V_PLACA3 IS INITIAL ).
    V_PLACA2 = V_PLACA3 .
    CLEAR: V_PLACA3.
  ENDIF.

  IF V_PLACA1 EQ V_PLACA2 OR
     V_PLACA1 EQ V_PLACA3 OR
     ( ( V_PLACA2 EQ V_PLACA3 ) AND  ( NOT V_PLACA2 IS INITIAL )  ) .
    MESSAGE I836(SD) WITH 'Não Preencher Placas Iquais.'.
    EXIT.
  ENDIF.

  IF V_PLACA4 IS NOT INITIAL.
    IF V_PLACA1 EQ V_PLACA4 OR V_PLACA2 EQ V_PLACA4 OR V_PLACA3 EQ V_PLACA4.
      MESSAGE I836(SD) WITH 'Não Preencher Placas Iquais.'.
      EXIT.
    ENDIF.
  ENDIF.

  IF WA_VT_L-ADD03 NE '0000000002'.

    IF NOT V_PLACA1 IS INITIAL.
      CLEAR: V_COUNTRY01,V_CIDADE01, V_PROPRIETARIO01.
      SELECT SINGLE COUNTRY TAXJURCODE PROPRIETARIO
        INTO (V_COUNTRY01,V_CIDADE01,V_PROPRIETARIO01)
        FROM ZLEST0002
       WHERE PC_VEICULO EQ V_PLACA1.

      IF ( V_COUNTRY01 NE V_COUNTRY )." or ( v_region ne v_cidade01(3) ).
        MESSAGE I836(SD) WITH 'País e UF da Placa' V_PLACA1 'não confere!'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
        EXPORTING
          P_KOART      = 'K'
          P_FILIAL     = WA_VT_L-TPLST
          P_FORNECEDOR = V_PROPRIETARIO01
        EXCEPTIONS
          ERROR        = 1
          BRANCH       = 2
          OTHERS       = 3.

      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.

    ENDIF.

    IF NOT V_PLACA2 IS INITIAL.
      CLEAR: V_COUNTRY01,V_CIDADE01, V_PROPRIETARIO01.
      SELECT SINGLE COUNTRY TAXJURCODE PROPRIETARIO
        INTO (V_COUNTRY01,V_CIDADE01,V_PROPRIETARIO01)
        FROM ZLEST0002
       WHERE PC_VEICULO EQ V_PLACA2.

      IF ( V_COUNTRY01 NE V_COUNTRY )." or ( v_region ne v_cidade01(3) ).
        MESSAGE I836(SD) WITH 'País e UF da Placa' V_PLACA2 'não confere!'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
        EXPORTING
          P_KOART      = 'K'
          P_FILIAL     = WA_VT_L-TPLST
          P_FORNECEDOR = V_PROPRIETARIO01
        EXCEPTIONS
          ERROR        = 1
          BRANCH       = 2
          OTHERS       = 3.

      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.

    ENDIF.

    IF NOT V_PLACA3 IS INITIAL.
      CLEAR: V_COUNTRY01,V_CIDADE01, V_PROPRIETARIO01.
      SELECT SINGLE COUNTRY TAXJURCODE PROPRIETARIO
        INTO (V_COUNTRY01,V_CIDADE01,V_PROPRIETARIO01)
        FROM ZLEST0002
       WHERE PC_VEICULO EQ V_PLACA3.

      IF ( V_COUNTRY01 NE V_COUNTRY )." or ( v_region ne v_cidade01(3) ).
        MESSAGE I836(SD) WITH 'País e UF da Placa' V_PLACA3 'não confere!'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
        EXPORTING
          P_KOART      = 'K'
          P_FILIAL     = WA_VT_L-TPLST
          P_FORNECEDOR = V_PROPRIETARIO01
        EXCEPTIONS
          ERROR        = 1
          BRANCH       = 2
          OTHERS       = 3.

      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.

    ENDIF.

    IF NOT V_PLACA4 IS INITIAL.
      CLEAR: V_COUNTRY01,V_CIDADE01, V_PROPRIETARIO01.
      SELECT SINGLE COUNTRY TAXJURCODE PROPRIETARIO
        INTO (V_COUNTRY01,V_CIDADE01,V_PROPRIETARIO01)
        FROM ZLEST0002
       WHERE PC_VEICULO EQ V_PLACA4.

      IF ( V_COUNTRY01 NE V_COUNTRY )." or ( v_region ne v_cidade01(3) ).
        MESSAGE I836(SD) WITH 'País e UF da Placa' V_PLACA3 'não confere!'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
        EXPORTING
          P_KOART      = 'K'
          P_FILIAL     = WA_VT_L-TPLST
          P_FORNECEDOR = V_PROPRIETARIO01
        EXCEPTIONS
          ERROR        = 1
          BRANCH       = 2
          OTHERS       = 3.

      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.

    ENDIF.

  ENDIF.

  PERFORM Z_PREENCHE_PLACA USING: V_PLACA1 V_CIDADE1,
                                  V_PLACA2 V_CIDADE2,
                                  V_PLACA3 V_CIDADE3,
                                  V_PLACA4 V_CIDADE4.

  LEAVE TO SCREEN 0.
ENDFORM.                    " Z_LEVA_VALORES

*&---------------------------------------------------------------------*
*&      Module  Z_PLACA1  INPUT                                        *
*&---------------------------------------------------------------------*
*                                 Placa 1                              *
*----------------------------------------------------------------------*
MODULE Z_PLACA1 INPUT.

  DATA: VL_PLACA         TYPE CHAR4,
        VL_LEN           TYPE I,
        WA_ZLEST0002_01  TYPE ZLEST0002,
        WA_J_1BTXJURT_01 TYPE J_1BTXJURT.

  DATA: WA_VT_1 TYPE VTTKVB.

  READ TABLE TI_XVTTK INTO WA_VT_1 INDEX 1.

  FIND REGEX '[A-Z]{3}[0-9]{1}[A-Z]{1}[0-9]{2}' IN V_PLACA1.
  IF SY-SUBRC IS NOT INITIAL.
    FIND REGEX '[A-Z]{3}[0-9]{4}' IN V_PLACA1.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE 'Placa 1 precisa ter o formato LLLNLNN ou LLLNNNN, onde L é letra e N é número.' TYPE 'E'.
    ENDIF.
  ENDIF.

*  VL_PLACA = V_PLACA1(03).
*  IF NOT VL_PLACA(03) CO SY-ABCDE.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_PLACA = V_PLACA1+03.
*  IF NOT VL_PLACA(04) CO '0123456789'.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_LEN = STRLEN( V_PLACA1 ).
*  IF NOT VL_LEN EQ 7.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.

  IF WA_VT_1-ADD03 NE '0000000002'.

    SELECT SINGLE * INTO WA_ZLEST0002_01
      FROM ZLEST0002
     WHERE PC_VEICULO EQ V_PLACA1.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E836(SD) WITH 'Placa do Cavalo' V_PLACA1 'não cadastrado!'.
    ENDIF.

    V_COUNTRY = WA_ZLEST0002_01-COUNTRY.
    V_REGION  = WA_ZLEST0002_01-TAXJURCODE(3).

    IF V_COUNTRY IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa do Cavalo não possui país definido!'.
    ENDIF.

    IF V_REGION IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa do Cavalo não possui UF definida!'.
    ENDIF.

    IF WA_ZLEST0002_01-TP_VEICULO NE '0'.
      MESSAGE E076(ZLES) WITH V_PLACA1.
    ENDIF.

    IF NOT PARCEIRO_PV IS INITIAL.
      IF WA_ZLEST0002_01-PROPRIETARIO NE PARCEIRO_PV.
        MESSAGE E836(SD) WITH 'Proprietário do Veículo' 'diferente de' PARCEIRO_PV '!'.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO WA_J_1BTXJURT_01
      FROM J_1BTXJURT
     WHERE SPRAS      = SY-LANGU
       AND COUNTRY    = WA_ZLEST0002_01-COUNTRY
       AND TAXJURCODE = WA_ZLEST0002_01-TAXJURCODE.

    IF SY-SUBRC IS INITIAL.
      CONCATENATE WA_J_1BTXJURT_01-TEXT '-' WA_J_1BTXJURT_01-TAXJURCODE(3) INTO V_CIDADE1.
      CLEAR: T_DYNPFIELDS[].
      MOVE: 'V_CIDADE1' TO T_DYNPFIELDS-FIELDNAME,
            V_CIDADE1   TO T_DYNPFIELDS-FIELDVALUE.
      APPEND T_DYNPFIELDS.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          DYNAME     = SY-REPID
          DYNUMB     = SY-DYNNR
        TABLES
          DYNPFIELDS = T_DYNPFIELDS.
    ENDIF.

  ENDIF.

ENDMODULE.                 " Z_PLACA1  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_PLACA2  INPUT                                        *
*&---------------------------------------------------------------------*
*                                 Placa 2                              *
*----------------------------------------------------------------------*
MODULE Z_PLACA2 INPUT.

  DATA: WA_VT_2 TYPE VTTKVB.

  READ TABLE TI_XVTTK INTO WA_VT_2 INDEX 1.

  FIND REGEX '[A-Z]{3}[0-9]{1}[A-Z]{1}[0-9]{2}' IN V_PLACA2.
  IF SY-SUBRC IS NOT INITIAL.
    FIND REGEX '[A-Z]{3}[0-9]{4}' IN V_PLACA2.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE 'Placa 2 precisa ter o formato LLLNLNN ou LLLNNNN, onde L é letra e N é número.' TYPE 'E'.
    ENDIF.
  ENDIF.

*  VL_PLACA = V_PLACA2(03).
*  IF NOT VL_PLACA(03) CO SY-ABCDE.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_PLACA = V_PLACA2+03.
*  IF NOT VL_PLACA(04) CO '0123456789'.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_LEN = STRLEN( V_PLACA2 ).
*  IF NOT VL_LEN EQ 7.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.

  IF WA_VT_2-ADD03 NE '0000000002'.

    SELECT SINGLE * INTO WA_ZLEST0002_01
      FROM ZLEST0002
     WHERE PC_VEICULO EQ V_PLACA2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA2 'não cadastrada!'.
    ENDIF.

    IF WA_ZLEST0002_01-COUNTRY IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA2 'não possui país definido!'.
    ENDIF.

    IF WA_ZLEST0002_01-TAXJURCODE IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA2 'não possui UF definida!'.
    ENDIF.

    IF WA_ZLEST0002_01-TP_VEICULO EQ '0'.
      MESSAGE E077(ZLES) WITH V_PLACA2.
    ENDIF.

*    if not parceiro_pv is initial.
*      if wa_zlest0002_01-proprietario ne parceiro_pv.
*        message e836(sd) with 'Proprietário do Veículo' 'diferente de' parceiro_pv '!'.
*      endif.
*    endif.

    SELECT SINGLE * INTO WA_J_1BTXJURT_01
      FROM J_1BTXJURT
     WHERE SPRAS      = SY-LANGU
       AND COUNTRY    = WA_ZLEST0002_01-COUNTRY
       AND TAXJURCODE = WA_ZLEST0002_01-TAXJURCODE.

    IF SY-SUBRC IS INITIAL.
      CONCATENATE WA_J_1BTXJURT_01-TEXT '-' WA_J_1BTXJURT_01-TAXJURCODE(3) INTO V_CIDADE2.
      CLEAR: T_DYNPFIELDS[].
      MOVE: 'V_CIDADE2' TO T_DYNPFIELDS-FIELDNAME,
            V_CIDADE2   TO T_DYNPFIELDS-FIELDVALUE.
      APPEND T_DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          DYNAME     = SY-REPID
          DYNUMB     = SY-DYNNR
        TABLES
          DYNPFIELDS = T_DYNPFIELDS.

    ENDIF.
  ENDIF.

ENDMODULE.                 " Z_PLACA2  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_PLACA3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_PLACA3 INPUT.

  DATA: WA_VT_3 TYPE VTTKVB.

  READ TABLE TI_XVTTK INTO WA_VT_3 INDEX 1.

  FIND REGEX '[A-Z]{3}[0-9]{1}[A-Z]{1}[0-9]{2}' IN V_PLACA3.
  IF SY-SUBRC IS NOT INITIAL.
    FIND REGEX '[A-Z]{3}[0-9]{4}' IN V_PLACA3.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE 'Placa 3 precisa ter o formato LLLNLNN ou LLLNNNN, onde L é letra e N é número.' TYPE 'E'.
    ENDIF.
  ENDIF.

*  VL_PLACA = V_PLACA3(03).
*  IF NOT VL_PLACA(03) CO SY-ABCDE.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_PLACA = V_PLACA3+03.
*  IF NOT VL_PLACA(04) CO '0123456789'.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_LEN = STRLEN( V_PLACA3 ).
*  IF NOT VL_LEN EQ 7.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.

  IF WA_VT_3-ADD03 NE '0000000002'.

    SELECT SINGLE * INTO WA_ZLEST0002_01
      FROM ZLEST0002
     WHERE PC_VEICULO EQ V_PLACA3.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA3 'não cadastrada!'.
    ENDIF.

    IF WA_ZLEST0002_01-COUNTRY IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA3 'não possui país definido!'.
    ENDIF.

    IF WA_ZLEST0002_01-TAXJURCODE IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA3 'não possui UF definida!'.
    ENDIF.

    IF WA_ZLEST0002_01-TP_VEICULO EQ '0'.
      MESSAGE E077(ZLES) WITH V_PLACA3.
    ENDIF.

*    if not parceiro_pv is initial.
*      if wa_zlest0002_01-proprietario ne parceiro_pv.
*        message e836(sd) with 'Proprietário do Veículo' 'diferente de' parceiro_pv '!'.
*      endif.
*    endif.

    SELECT SINGLE * INTO WA_J_1BTXJURT_01
      FROM J_1BTXJURT
     WHERE SPRAS      = SY-LANGU
       AND COUNTRY    = WA_ZLEST0002_01-COUNTRY
       AND TAXJURCODE = WA_ZLEST0002_01-TAXJURCODE.

    IF SY-SUBRC IS INITIAL.
      CONCATENATE WA_J_1BTXJURT_01-TEXT '-' WA_J_1BTXJURT_01-TAXJURCODE(3) INTO V_CIDADE3.
      CLEAR: T_DYNPFIELDS[].
      MOVE: 'V_CIDADE3' TO T_DYNPFIELDS-FIELDNAME,
            V_CIDADE3   TO T_DYNPFIELDS-FIELDVALUE.
      APPEND T_DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          DYNAME     = SY-REPID
          DYNUMB     = SY-DYNNR
        TABLES
          DYNPFIELDS = T_DYNPFIELDS.
    ENDIF.

  ENDIF.

ENDMODULE.                 " Z_PLACA3  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_PLACA4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_PLACA4 INPUT.

  DATA: WA_VT_4 TYPE VTTKVB.

  READ TABLE TI_XVTTK INTO WA_VT_4 INDEX 1.

  FIND REGEX '[A-Z]{3}[0-9]{1}[A-Z]{1}[0-9]{2}' IN V_PLACA4.
  IF SY-SUBRC IS NOT INITIAL.
    FIND REGEX '[A-Z]{3}[0-9]{4}' IN V_PLACA4.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE 'Placa 4 precisa ter o formato LLLNLNN ou LLLNNNN, onde L é letra e N é número.' TYPE 'E'.
    ENDIF.
  ENDIF.

*  VL_PLACA = V_PLACA3(03).
*  IF NOT VL_PLACA(03) CO SY-ABCDE.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_PLACA = V_PLACA3+03.
*  IF NOT VL_PLACA(04) CO '0123456789'.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.
*  VL_LEN = STRLEN( V_PLACA3 ).
*  IF NOT VL_LEN EQ 7.
*    MESSAGE E836(SD) WITH 'Placa Precisa ter o Formato AAA1111'.
*  ENDIF.

  IF WA_VT_4-ADD03 NE '0000000002'.

    SELECT SINGLE * INTO WA_ZLEST0002_01
      FROM ZLEST0002
     WHERE PC_VEICULO EQ V_PLACA4.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA4 'não cadastrada!'.
    ENDIF.

    IF WA_ZLEST0002_01-COUNTRY IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA4 'não possui país definido!'.
    ENDIF.

    IF WA_ZLEST0002_01-TAXJURCODE IS INITIAL.
      MESSAGE E836(SD) WITH 'Placa da Carreta' V_PLACA4 'não possui UF definida!'.
    ENDIF.

    IF WA_ZLEST0002_01-TP_VEICULO EQ '0'.
      MESSAGE E077(ZLES) WITH V_PLACA4.
    ENDIF.

*    if not parceiro_pv is initial.
*      if wa_zlest0002_01-proprietario ne parceiro_pv.
*        message e836(sd) with 'Proprietário do Veículo' 'diferente de' parceiro_pv '!'.
*      endif.
*    endif.

    SELECT SINGLE * INTO WA_J_1BTXJURT_01
      FROM J_1BTXJURT
     WHERE SPRAS      = SY-LANGU
       AND COUNTRY    = WA_ZLEST0002_01-COUNTRY
       AND TAXJURCODE = WA_ZLEST0002_01-TAXJURCODE.

    IF SY-SUBRC IS INITIAL.
      CONCATENATE WA_J_1BTXJURT_01-TEXT '-' WA_J_1BTXJURT_01-TAXJURCODE(3) INTO V_CIDADE4.
      CLEAR: T_DYNPFIELDS[].
      MOVE: 'V_CIDADE4' TO T_DYNPFIELDS-FIELDNAME,
            V_CIDADE4   TO T_DYNPFIELDS-FIELDVALUE.
      APPEND T_DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          DYNAME     = SY-REPID
          DYNUMB     = SY-DYNNR
        TABLES
          DYNPFIELDS = T_DYNPFIELDS.
    ENDIF.

  ENDIF.

ENDMODULE.                 " Z_PLACA3  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0200  INPUT                                  *
*&---------------------------------------------------------------------*
*                         User Exit Command                            *
*----------------------------------------------------------------------*
MODULE USER_EXIT_0200 INPUT.

  CASE SY-UCOMM.
    WHEN 'BT_CANC'.
      "if not v_placa1 is initial and not v_placa2 is initial.
      LEAVE TO SCREEN 0.
      "endif.
  ENDCASE.

ENDMODULE.                 " USER_EXIT_0200  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_PLACA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM Z_PREENCHE_PLACA USING P_PLACA  TYPE CHAR7
                            P_CIDADE TYPE TEXT60.

  DATA SL_PLACA TYPE ZLESE0032.

  SL_PLACA-PLACA  = P_PLACA.
  SL_PLACA-PAIS   = V_COUNTRY.
  SL_PLACA-ESTADO = V_REGION.
  SL_PLACA-CIDADE = P_CIDADE.

  APPEND SL_PLACA TO TI_PLACA.

ENDFORM.                    " Z_PREENCHE_PLACA

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  DATA: WA_VT_S TYPE VTTKVB.

  READ TABLE TI_XVTTK INTO WA_VT_S INDEX 1.

  IF WA_VT_S-ADD03 EQ '0000000002'.
    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ 'V_COUNTRY' ) OR
         ( SCREEN-NAME EQ 'V_REGION' ) OR
         ( SCREEN-NAME EQ 'V_CIDADE1' ) OR
         ( SCREEN-NAME EQ 'V_CIDADE2' ) OR
         ( SCREEN-NAME EQ 'V_CIDADE3' ).
        SCREEN-INPUT    = '1'.
        IF ( SCREEN-NAME NE 'V_CIDADE3' ) AND ( SCREEN-NAME NE 'V_CIDADE2' ).
          SCREEN-REQUIRED = '1'.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SET PF-STATUS 'PF0200'.

  IF WA_VT_S-ADD03 EQ '0000000001'.
    IF ( NOT V_PLACA1 IS INITIAL ) AND ( V_COUNTRY IS INITIAL ) AND ( V_REGION IS INITIAL ) AND ( V_CIDADE1 IS INITIAL ).

      SELECT SINGLE * INTO WA_ZLEST0002_01
        FROM ZLEST0002
       WHERE PC_VEICULO EQ V_PLACA1.

      V_COUNTRY = WA_ZLEST0002_01-COUNTRY.
      V_REGION  = WA_ZLEST0002_01-TAXJURCODE(3).

      IF ( V_COUNTRY IS INITIAL ) AND ( V_REGION IS INITIAL ).

        SELECT SINGLE * INTO WA_J_1BTXJURT_01
          FROM J_1BTXJURT
         WHERE SPRAS      = SY-LANGU
           AND COUNTRY    = WA_ZLEST0002_01-COUNTRY
           AND TAXJURCODE = WA_ZLEST0002_01-TAXJURCODE.

        IF SY-SUBRC IS INITIAL.
          CONCATENATE WA_J_1BTXJURT_01-TEXT '-' WA_J_1BTXJURT_01-TAXJURCODE(3) INTO V_CIDADE1.
        ENDIF.

      ENDIF.

    ENDIF.

    IF ( NOT V_PLACA2 IS INITIAL ) AND ( V_CIDADE2 IS INITIAL ).

      CLEAR: WA_ZLEST0002_01,
             WA_J_1BTXJURT_01.

      SELECT SINGLE * INTO WA_ZLEST0002_01
        FROM ZLEST0002
       WHERE PC_VEICULO EQ V_PLACA2.

      V_COUNTRY = WA_ZLEST0002_01-COUNTRY.
      V_REGION  = WA_ZLEST0002_01-TAXJURCODE(3).

      IF ( V_COUNTRY IS INITIAL ) AND ( V_REGION IS INITIAL ).

        SELECT SINGLE * INTO WA_J_1BTXJURT_01
          FROM J_1BTXJURT
         WHERE SPRAS      = SY-LANGU
           AND COUNTRY    = WA_ZLEST0002_01-COUNTRY
           AND TAXJURCODE = WA_ZLEST0002_01-TAXJURCODE.

        IF SY-SUBRC IS INITIAL.
          CONCATENATE WA_J_1BTXJURT_01-TEXT '-' WA_J_1BTXJURT_01-TAXJURCODE(3) INTO V_CIDADE2.
        ENDIF.

      ENDIF.

    ENDIF.

    IF ( NOT V_PLACA3 IS INITIAL ) AND ( V_CIDADE3 IS INITIAL ).

      CLEAR: WA_ZLEST0002_01,
             WA_J_1BTXJURT_01.

      SELECT SINGLE * INTO WA_ZLEST0002_01
        FROM ZLEST0002
       WHERE PC_VEICULO EQ V_PLACA3.

      V_COUNTRY = WA_ZLEST0002_01-COUNTRY.
      V_REGION  = WA_ZLEST0002_01-TAXJURCODE(3).

      IF ( V_COUNTRY IS INITIAL ) AND ( V_REGION IS INITIAL ).

        SELECT SINGLE * INTO WA_J_1BTXJURT_01
          FROM J_1BTXJURT
         WHERE SPRAS      = SY-LANGU
           AND COUNTRY    = WA_ZLEST0002_01-COUNTRY
           AND TAXJURCODE = WA_ZLEST0002_01-TAXJURCODE.

        IF SY-SUBRC IS INITIAL.
          CONCATENATE WA_J_1BTXJURT_01-TEXT '-' WA_J_1BTXJURT_01-TAXJURCODE(3) INTO V_CIDADE3.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_CHECK_CAMPOS_OBRIGATORIOS
*&---------------------------------------------------------------------*
FORM Z_CHECK_CAMPOS_OBRIGATORIOS.

* IF ZLEST0028-CODTRP IS INITIAL.
*   MESSAGE 'Código de transportadora de preenchimento obrigatório'
*      TYPE 'E'.
* ELSEIF ZLEST0028-CHVID IS INITIAL.
*   MESSAGE 'Chave de Identificação de preenchimento obrigatório'
*      TYPE 'E'.
* ELSEIF ZLEST0028-CONHEC IS INITIAL.
*   MESSAGE 'Conhecimento de preenchimento obrigatório'
*      TYPE 'E'.
* ELSEIF ZLEST0028-CTAFRETE IS INITIAL.
*   MESSAGE 'Carta Frete de preenchimento obrigatório'
*      TYPE 'E'.
* ELSEIF ZLEST0028-VALOR IS INITIAL.
*   MESSAGE 'Valor reembolso de preenchimento obrigatório'
*      TYPE 'E'.
* ENDIF.

ENDFORM.                    "Z_CHECK_CAMPOS_OBRIGATORIOS

*&---------------------------------------------------------------------*
*&      Form  Z_010_GERA_DATA_HORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM Z_010_GERA_DATA_HORA.

  ZLEST0010-DATA    = SY-DATUM.
  ZLEST0010-HORA    = SY-UZEIT.
  ZLEST0010-USUARIO = SY-UNAME.

ENDFORM.                    "Z_010_GERA_DATA_HORA
*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_FORNECEDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCA_NOME_FORNECEDOR INPUT.
  DATA: WNM_FORNECEDOR TYPE LFA1-NAME1.

  SELECT SINGLE NAME1
    INTO WNM_FORNECEDOR
    FROM LFA1
    WHERE LIFNR EQ ZLEST0019-COD_FORNECEDOR.

  NM_FORNECEDOR = WNM_FORNECEDOR.

ENDMODULE.                 " BUSCA_NOME_FORNECEDOR  INPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCA_NOME_FORNECEDOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCA_NOME_FORNECEDOR OUTPUT.
  DATA: WNM_FORNECEDOR2 TYPE LFA1-NAME1.

  CLEAR : WNM_FORNECEDOR2.

  SELECT SINGLE NAME1
    INTO WNM_FORNECEDOR2
    FROM LFA1
    WHERE LIFNR EQ ZLEST0019-COD_FORNECEDOR.

  NM_FORNECEDOR = WNM_FORNECEDOR2.
ENDMODULE.                 " BUSCA_NOME_FORNECEDOR  OUTPUT

FORM CTE_IS_AUTHORIZED USING DOCNUM AUTHORIZED.
  SELECT SINGLE *
    FROM J_1BNFE_ACTIVE
    INTO @DATA(_STATUS_CTE)
   WHERE DOCNUM = @DOCNUM
     AND DOCSTA = '1'
     AND CANCEL = @SPACE.

  IF SY-SUBRC IS INITIAL.
    MOVE ABAP_TRUE  TO AUTHORIZED.
  ELSE.
    MOVE ABAP_FALSE TO AUTHORIZED.
  ENDIF.
ENDFORM.

FORM DATE_AND_TIME_TO_EXTERNAL USING DATE TIME EXTERNAL_DATE.
  "//XML Format 2017-03-16T09:21:28Z
  EXTERNAL_DATE = |{ DATE+0(4) }-{ DATE+4(2) }-{ DATE+6(2) }T{ TIME+0(2) }:{ TIME+2(2) }:{ TIME+4(2) }Z|.
ENDFORM.

FORM MONTAR_XML_LOGONE USING COMBOIO    TYPE ZLESE0005
                             CD_DESTINO TYPE ZCHAR02
                             USERNAME
                             PASSWORD
                             XML_RETORNO.
  DATA:
    BEGIN OF XML,
      BARCACAS    TYPE STRING,
      NF_BARCACA  TYPE STRING,
      REBOCADORES TYPE STRING,
    END OF XML.

  DATA: V_SAP_INTERFACES TYPE STRING.

  LOOP AT COMBOIO-BARCACAS INTO DATA(_BARCACA).
    LOOP AT _BARCACA-NOTASFISCAIS INTO DATA(_NOTA_BARCACA).
      XML-NF_BARCACA = XML-NF_BARCACA                                  &&
        |<NotaFiscalVO>|                                               &&
           |<chaveNfe>{ _NOTA_BARCACA-CHAVENFE }</chaveNfe>|           &&
           |<peso>{ _NOTA_BARCACA-PESO }</peso>|                       &&
           |<pesoTotal>{ _NOTA_BARCACA-PESO_TOTAL }</pesoTotal>|       &&
           "|<situacaoNota>{ _NOTA_BARCACA-ST_EXP }</situacaoNota>|     &&
        |</NotaFiscalVO>|.
    ENDLOOP.

    XML-BARCACAS = XML-BARCACAS                                        &&
      |<BarcacaVO>|                                                    &&
         |<identificador>{ _BARCACA-IDENTIFICADOR }</identificador>|   &&
         |{ XML-NF_BARCACA }|                                          &&
      |</BarcacaVO>|.

    CLEAR XML-NF_BARCACA.
  ENDLOOP.

  LOOP AT COMBOIO-REBOCADORES INTO DATA(_REBOCADOR).
    XML-REBOCADORES = XML-REBOCADORES                                  &&
      |<RebocadorVO>|                                                  &&
         |<identificador>{ _REBOCADOR-IDENTIFICADOR }</identificador>| &&
      |</RebocadorVO>|.
  ENDLOOP.

  CLEAR: V_SAP_INTERFACES.
  CASE CD_DESTINO.
    WHEN '01'. "Barcarena
      V_SAP_INTERFACES = 'terfron'.
    WHEN '02'. "Itaituba
      V_SAP_INTERFACES = 'itaituba'.
  ENDCASE.

  CONCATENATE
 '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sap="http://sap.interfaces.' V_SAP_INTERFACES '.logOne.arcadian.com/">'
   '<soapenv:Header>'
      '<wsse:Security soapenv:mustUnderstand="1" xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">'
         '<wsse:UsernameToken wsu:Id="UsernameToken-1">'
            '<wsse:Username>' USERNAME '</wsse:Username>'
            '<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">' PASSWORD '</wsse:Password>'
         '</wsse:UsernameToken>'
      '</wsse:Security>'
   '</soapenv:Header>'
   '|<soapenv:Body>|'
      '<sap:registrarRecebimentoComboioEmTransito>'
         '<comboioVO>'
            '<identificador>' COMBOIO-IDENTIFICADOR '</identificador>'
            '<cnpjTransportador>' COMBOIO-CNPJ_TRANSPORTADOR '</cnpjTransportador>'
            '<dataSaidaOrigem>' COMBOIO-DATA_SAIDA_ORIGEM '</dataSaidaOrigem>'
            '<dataPrevisaoChegada>' COMBOIO-DATA_PREVISAO_CHEGADA '</dataPrevisaoChegada>'
            XML-REBOCADORES
            XML-BARCACAS
         '</comboioVO>'
      '</sap:registrarRecebimentoComboioEmTransito>'
   '</soapenv:Body>'
 '</soapenv:Envelope>' INTO XML_RETORNO.
ENDFORM.

FORM MONTAR_XML_BUNGE USING COMBOIO TYPE ZLESE0005 USERNAME PASSWORD XML_RETORNO.
  DATA:
    BEGIN OF XML,
      BARCACAS    TYPE STRING,
      NF_BARCACA  TYPE STRING,
      REBOCADORES TYPE STRING,
    END OF XML.

  XML-BARCACAS = '<Barcacas>'.

  LOOP AT COMBOIO-BARCACAS INTO DATA(_BARCACA).
    XML-NF_BARCACA = '<NotasFiscais>'.

    LOOP AT _BARCACA-NOTASFISCAIS INTO DATA(_NOTA_BARCACA).
      XML-NF_BARCACA = XML-NF_BARCACA                                                 &&
        |<NotaFiscal>|                                                                &&
           |<chaveNfe>{ _NOTA_BARCACA-CHAVENFE }</chaveNfe>|                          &&
           |<peso>{ _NOTA_BARCACA-PESO }</peso>|                                      &&
           |<CnpjRemetente>{ _NOTA_BARCACA-CNPJ_REMETENTE }</CnpjRemetente>|          &&
           |<CnpjDestinatario>{ _NOTA_BARCACA-CNPJ_DESTINATARIO }</CnpjDestinatario>| &&
           |<Numero>{ _NOTA_BARCACA-NUMERO }</Numero>|                                &&
           |<Serie>{ _NOTA_BARCACA-SERIE }</Serie>|                                   &&
           |<CodigoProduto>{ _NOTA_BARCACA-CODIGO_PRODUTO }</CodigoProduto>|          &&
           |<DescricaoProduto>{ _NOTA_BARCACA-DESCRICAO_PRODUTO }</DescricaoProduto>| &&
           |<Valor>{ _NOTA_BARCACA-VALOR }</Valor>|                                   &&
           |<ChaveNotaEletronica>{ _NOTA_BARCACA-CHAVENFE }</ChaveNotaEletronica>|    &&
        |</NotaFiscal>|.
    ENDLOOP.

    XML-NF_BARCACA = XML-NF_BARCACA && '</NotasFiscais>'.

    XML-BARCACAS = XML-BARCACAS                                                       &&
      |<Barcaca>|                                                                       &&
         |<Codigo>{ _BARCACA-IDENTIFICADOR }</Codigo>|                                &&
         |<CnpjTransportadora>{ _BARCACA-CNPJ_TRANSPORTADORA }</CnpjTransportadora>|  &&
         |<PesoTara>{ _BARCACA-PESO_TARA }</PesoTara>|                                &&
         |<PesoLiquido>{  _BARCACA-PESO_BRUTO }</PesoLiquido>|                        &&
         |<PesoBruto>{ _BARCACA-PESO_BRUTO }</PesoBruto>|                             &&
         |<DataCarregamento>{ _BARCACA-DATA_CARREGAMENTO }</DataCarregamento>|        &&
         |<LocalCarregamento>{ _BARCACA-LOCAL_CARREGAMENTO }</LocalCarregamento>|     &&
         |{ XML-NF_BARCACA }|                                                         &&
      |</Barcaca>|.

  ENDLOOP.

  XML-BARCACAS = XML-BARCACAS && '</Barcacas>'.

  CONCATENATE
  '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:edi="http://edi.bunge.com.br/">'

   '<soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing" >'
    '<m:Partner xmlns:m="http://edi.bunge.com.br/partner/">23771214000248</m:Partner>'
    '<m:LogisticProcess xmlns:m="http://edi.bunge.com.br/logistic-process/">CRM_TRANSBORDO</m:LogisticProcess>'
    '<m:AppKey xmlns:m="http://edi.bunge.com.br/app-key/">amaggi</m:AppKey>   '
    '<wsa:Action>http://edi.bunge.com.br/Edi/ReceiveXmlDocument</wsa:Action>'
   '</soap:Header>'
   '<soap:Body>'
    '<ReceiveXmlDocument xmlns="http://edi.bunge.com.br/">'
     '<document>'
      '<CarregamentoHidroviarioInput xmlns="http://xmlns.edi.com.br/hidrovias/CarregamentoHidroviarioInput">'
        '<CabecalhoEntrega>'
          '<identificador>' COMBOIO-IDENTIFICADOR '</identificador>'
          '<dataSaidaOrigem>' COMBOIO-DATA_SAIDA_ORIGEM '</dataSaidaOrigem>'
          '<dataPrevisaoChegada>' COMBOIO-DATA_PREVISAO_CHEGADA '</dataPrevisaoChegada>'
          '<ProtocoloEnvio>' COMBOIO-IDENTIFICADOR '</ProtocoloEnvio>'
          '<TipoEnvio>CRM_TRANSBORDO</TipoEnvio>'
          '<Transacao>' COMBOIO-TRANSACAO '</Transacao>'
          '<DataHoraGeracao>' COMBOIO-DATA_HORA_GERACAO '</DataHoraGeracao>'
          '<Remetente>'
            '<IdentificacaoEmpresa>' COMBOIO-CNPJ_REMETENTE '</IdentificacaoEmpresa>'
            '<RazaoSocial>' COMBOIO-EMPRESA_REMETENTE '</RazaoSocial>'
          '</Remetente>'
          '<Recebedor>'
            '<IdentificacaoEmpresa>' COMBOIO-CNPJ_DESTINATARIO '</IdentificacaoEmpresa>'
            '<RazaoSocial>' COMBOIO-EMPRESA_DESTINATARIO '</RazaoSocial>'
          '</Recebedor>'
         '</CabecalhoEntrega>'

         '<CarregamentoHidroviarioDetalhes>'
          '<Acao>carga</Acao>'
            XML-BARCACAS
         '</CarregamentoHidroviarioDetalhes>'
      '</CarregamentoHidroviarioInput>'

      '</document>'
     '</ReceiveXmlDocument>'
   '</soap:Body>'
  '</soap:Envelope>'

 INTO XML_RETORNO.
ENDFORM.
