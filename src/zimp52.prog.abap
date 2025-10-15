*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 14/02/2013                                              &*
*& Descrição: Cadastro de Lote de Impostos                            &*
*& Transação: ZIMP52                                                  &*
*---------------------------------------------------------------------&*
*& Histórico de alterações                                            &*
*& Autor            Request       Data                                &*
*& Marcos Faneli    DEVK937325    02.05.2014                          &*
*&--------------------------------------------------------------------&*

REPORT  ZIMP52.

TYPES: BEGIN OF TY_CADLOT,
         LOTE       TYPE ZIMP_CAD_LOTE-LOTE,
         BUKRS      TYPE ZIMP_CAD_LOTE-BUKRS,
         DESCR_LOTE TYPE ZIMP_CAD_LOTE-DESCR_LOTE,
         USNAM      TYPE ZIMP_CAD_LOTE-USNAM,
         DEP_RESP   TYPE ZIMP_CAD_LOTE-DEP_RESP,
         DT_VENC    TYPE ZIMP_CAD_LOTE-DT_VENC,
       END OF TY_CADLOT,

       BEGIN OF TY_FIELDS,
         CAMPO(30) TYPE C,
         GROUP1(5) TYPE C,
         VALUE     TYPE SY-TABIX,
         INVISIBLE TYPE SY-TABIX,
       END OF TY_FIELDS.

DATA: MSG_ERRO(6) TYPE C.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: WG_CADLOT  TYPE TY_CADLOT,
      TG_FIELDS  TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_MSG_RET TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Declaração variaveis diversas                                      &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      OK_CODE         LIKE SY-UCOMM,
      MSG_TEXT        TYPE STRING, "- #125580 SMC
      WG_MENSAGEM(30),
      WG_ACAO(30).

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0               TYPE C VALUE '0',
           C_1               TYPE C VALUE '1',
           C_2               TYPE C VALUE '2',
           C_B               TYPE C VALUE 'B',
           C_S               TYPE C VALUE 'S',
           C_L               TYPE C VALUE 'L',
           C_X               TYPE C VALUE 'X',
           C_D               TYPE C VALUE 'D',
           C_K               TYPE C VALUE 'K',
           C_W               TYPE C VALUE 'W',
           C_F               TYPE C VALUE 'F',
           C_T               TYPE C VALUE 'T',
           C_I               TYPE C VALUE 'I',
           C_N               TYPE C VALUE 'N',
           C_H               TYPE C VALUE 'H',
           C_AG(2)           TYPE C VALUE 'AG',
           C_NE(2)           TYPE C VALUE 'NE',
           C_01(2)           TYPE C VALUE '01',
           C_30(2)           TYPE C VALUE '30',
           C_40(2)           TYPE C VALUE '40',
           C_50(4)           TYPE C VALUE '0050',
           C_76(2)           TYPE C VALUE '76',
           C_71(2)           TYPE C VALUE '71',
           C_72(2)           TYPE C VALUE '72',
           C_BR(2)           TYPE C VALUE 'BR',
           C_LF(2)           TYPE C VALUE 'LF',
           C_LR(2)           TYPE C VALUE 'LR',
           C_Z1(2)           TYPE C VALUE 'Z1',
           C_ADD(3)          TYPE C VALUE 'ADD',
           C_DEL(3)          TYPE C VALUE 'DEL',
           C_DG1(3)          TYPE C VALUE 'DG1',
           C_DG2(3)          TYPE C VALUE 'DG2',
           C_DUMMY_HEADER(3) TYPE C VALUE '099',
           C_DUMMY_ITENS(3)  TYPE C VALUE '098',
           C_EXIT(4)         TYPE C VALUE 'EXIT',
           C_ROOT(4)         TYPE C VALUE 'ROOT',
           C_MINIMIZAR(4)    TYPE C VALUE '@K2@',
           C_MAXIMIZAR(4)    TYPE C VALUE '@K1@',
           C_BACK(4)         TYPE C VALUE 'BACK',
           C_SAVE(4)         TYPE C VALUE 'SAVE',
           C_DESAT(5)        TYPE C VALUE 'DESAT',
           C_DMBTR(5)        TYPE C VALUE 'DMBTR',
           C_MODIF(5)        TYPE C VALUE 'MODIF',
           C_CANCEL(6)       TYPE C VALUE 'CANCEL',
           C_DELDOC(6)       TYPE C VALUE 'DELDOC',
           C_DCLICK(6)       TYPE C VALUE 'DCLICK',
           C_SEARCH(6)       TYPE C VALUE 'SEARCH',
           C_ATUALI(6)       TYPE C VALUE 'ATUALI',
           C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
           C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
           C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
           C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
           C_LANC(10)        TYPE C VALUE 'LANC',
           C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH: FCODE.
  DATA: WL_ZIMP_CAD_LOTE2      TYPE ZIMP_CAD_LOTE.

  IF WG_CADLOT-LOTE IS NOT INITIAL.
    SELECT SINGLE *
      FROM ZIMP_CAD_LOTE
      INTO WL_ZIMP_CAD_LOTE2
       WHERE  LOTE EQ WG_CADLOT-LOTE.
    IF SY-SUBRC IS NOT INITIAL.
    ELSEIF WL_ZIMP_CAD_LOTE2-STATUS_LOTE = 'L' OR  WL_ZIMP_CAD_LOTE2-STATUS_LOTE = 'A'.
      APPEND C_MODIF TO FCODE.
    ENDIF.
  ENDIF.

  IF WG_ACAO IS INITIAL.
    APPEND C_SAVE TO FCODE.
    APPEND C_DELDOC TO FCODE.
  ENDIF.
  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET TITLEBAR '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  DATA: V_LOTE_ZIMP TYPE ZIMP_CAD_LOTE-LOTE.

  DATA W_ANSWER.
  CASE OK-CODE.
    WHEN C_LANC.
      CLEAR: V_LOTE_ZIMP.
      GET PARAMETER ID 'LOTEZIMP' FIELD V_LOTE_ZIMP.
      IF V_LOTE_ZIMP NE WG_CADLOT-LOTE OR WG_CADLOT-LOTE IS INITIAL.
        MESSAGE TEXT-I01 TYPE 'I'.
      ELSE.
        CALL TRANSACTION 'ZIMP53' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN C_DELDOC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          TEXT_QUESTION         = 'Confirma a exclusão do Lote?'
          TEXT_BUTTON_1         = 'Sim'(001)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(002)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
*         USERDEFINED_F1_HELP   = ' '
          START_COLUMN          = 25
          START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          ANSWER                = W_ANSWER
*       TABLES
*         PARAMETER             =
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
      .
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        PERFORM ELIMINAR_LOTE.
      ENDIF.
    WHEN C_SEARCH.
      PERFORM BUSCA_DADOS.
    WHEN C_SAVE.
      AUTHORITY-CHECK OBJECT 'F_SKA1_BUK'
         ID 'ACTVT' FIELD '03'       "display
         ID 'BUKRS' FIELD WG_CADLOT-BUKRS.
      IF SY-SUBRC <> 0.
        MESSAGE 'Sem autorização para esta Empresa ' TYPE 'E'.
        EXIT.
      ENDIF.
      PERFORM VERIFICA_ERROS.


      IF TG_MSG_RET[] IS INITIAL. "AND msg_erro IS INITIAL. SMC
        CLEAR WG_ACAO.
        PERFORM GRAVA_DADOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_BACK.
      CLEAR WG_ACAO.
    WHEN C_ADD.
      WG_ACAO = C_MODIF.
      PERFORM LIMPA_CAMPOS.
      PERFORM OBTEM_PROXIMO.
      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM TRATA_CAMPOS USING SPACE
                                'GR1'
                                 C_0       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0


    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

      ELSE.
        WG_ACAO = C_MODIF.
        PERFORM TRATA_CAMPOS USING SPACE
                                   'GR2'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_0       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

      ENDIF.
    WHEN C_SHOW_MSGRE.
      CLEAR WG_ACAO.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_EXIT.

      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.

    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
  DATA: WL_ZIMP_CAD_LOTE      TYPE ZIMP_CAD_LOTE.


  IF WG_CADLOT-LOTE IS NOT INITIAL AND WG_ACAO IS INITIAL.
    SELECT SINGLE *
      FROM ZIMP_CAD_LOTE
      INTO WL_ZIMP_CAD_LOTE
       WHERE  LOTE EQ WG_CADLOT-LOTE.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Nº de Lote não encontrado!'.
      LEAVE TO SCREEN 100.
    ELSEIF WL_ZIMP_CAD_LOTE-STATUS_LOTE = 'L'.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Lote já liberado!'.
      MOVE-CORRESPONDING WL_ZIMP_CAD_LOTE TO WG_CADLOT.
      "LEAVE TO SCREEN 100.
    ELSEIF WL_ZIMP_CAD_LOTE-STATUS_LOTE = 'A'.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Lote já finalizado!'.
      MOVE-CORRESPONDING WL_ZIMP_CAD_LOTE TO WG_CADLOT.
      "LEAVE TO SCREEN 100.
    ELSE.
      MOVE-CORRESPONDING WL_ZIMP_CAD_LOTE TO WG_CADLOT.
    ENDIF.
    CLEAR WG_ACAO.
    REFRESH: TG_FIELDS.
    PERFORM TRATA_CAMPOS USING SPACE
                               'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM TRATA_CAMPOS USING SPACE
                              'GR1'
                               C_1       "INPUT 1     NO INPUT 0
                               C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .

  REFRESH: TG_MSG_RET.
  CLEAR: TG_MSG_RET.
  DATA: TL_T001           TYPE TABLE OF T001 WITH HEADER LINE.

  DATA: WA_USR   LIKE V_USR_NAME,
        WA_DEPTO LIKE ZIMP_CAD_DEPTO.

  SELECT *
    FROM T001
    INTO TABLE TL_T001
     WHERE BUKRS EQ WG_CADLOT-BUKRS.

  SORT TL_T001 BY BUKRS.

  IF WG_CADLOT-DESCR_LOTE IS INITIAL.
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          "c_tab_strip_imp-tab1      TO tg_msg_ret-aba,
          'WG_CADLOT-DESCR_LOTE'    TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Descrição do Lote' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.


  IF WG_CADLOT-BUKRS IS INITIAL.
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          "c_tab_strip_imp-tab1      TO tg_msg_ret-aba,
          'WG_CADLOT-BUKRS'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Empresa' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    READ TABLE TL_T001
          WITH KEY BUKRS = WG_CADLOT-BUKRS
                   BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E02 ' Empresa '  INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLOT-USNAM IS INITIAL.
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLOT-USNAM'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Usuário' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
      FROM  V_USR_NAME INTO WA_USR
              WHERE BNAME = WG_CADLOT-USNAM.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E02 ' Usuário '  INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLOT-DEP_RESP IS INITIAL.
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLOT-DEP_RESP'      TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Departamento Responsável' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
    "ELSEIF NOT '01_02_03_04_05_06_07_08_09' CS wg_cadlot-dep_resp OR wg_cadlot-dep_resp+0(1) NE '0' .
  ELSE.
    SELECT SINGLE *
     FROM  ZIMP_CAD_DEPTO INTO WA_DEPTO
             WHERE DEP_RESP = WG_CADLOT-DEP_RESP.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E02 'Departamento Responsável ' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSEIF WA_DEPTO-INATIVO = 'S'.
      CONCATENATE 'Departamento Inativo ' '.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLOT-DT_VENC IS  INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLOT-DT_VENC'   TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Dt. Vencto' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ENDIF.

*   " 125580-CS2023000810 Ajuste ZIMP (Data de Vencimento) - SMC - Inicio <<< - Equalização ECC x Hana
*  TRY.
*      CALL METHOD zcl_miro=>verificar_vencimento_fatura
*        EXPORTING
*          i_data_vencimento = wg_cadlot-dt_venc
*          i_pymt_meth       = ' '
*          i_data_se         = ' '
*          i_ck_revisao      = 'X'.
*
*    CATCH zcx_miro_exception INTO DATA(ex_miro).
*      ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).


  ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_CADLOT-DT_VENC
                              IMPORTING E_SUBRC      = DATA(IS_DIA_UTIL)
                               ).

  IF IS_DIA_UTIL <> 4 AND WG_CADLOT-DT_VENC IS NOT INITIAL.

*        MESSAGE 'Data informada não é dia útil' TYPE 'I'.

*        DATA:
*          e_msg_id   LIKE  t100-arbgb,
*          e_msg_no   LIKE  t100-msgnr,
*          e_msg_var1 LIKE  balm-msgv1,
*          e_msg_var2 LIKE  balm-msgv2,
*          e_msg_var3 LIKE  balm-msgv3,
*          e_msg_var4 LIKE  balm-msgv4.
*
*        e_msg_id = sy-msgid.
*        e_msg_no = sy-msgno.
*        e_msg_var1 = sy-msgv1.
*        e_msg_var2 = sy-msgv2.
*        e_msg_var3 = sy-msgv3.
*        e_msg_var4 = sy-msgv4.


*        CALL FUNCTION 'MESSAGE_PREPARE'
*          EXPORTING
*            msg_id                 = e_msg_id
*            msg_no                 = e_msg_no
*            msg_var1               = e_msg_var1
*            msg_var2               = e_msg_var2
*            msg_var3               = e_msg_var3
*            msg_var4               = e_msg_var4
*          IMPORTING
*            msg_text               = tg_msg_ret-msg
*          EXCEPTIONS
*            function_not_completed = 1
*            message_not_found      = 2
*            OTHERS                 = 3.
    CONCATENATE 'Data informanda' WG_CADLOT-DT_VENC 'não é dia útil' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
  ENDIF.
*    CLEAR: tg_msg_ret.
*    CATCH zcx_error INTO ex_erro.
*      ex_erro->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*      EXIT.
*  ENDTRY.
*  " 125580-CS2023000810 Ajuste ZIMP (Data de Vencimento) - SMC - fim <<< - Equalização ECC x Hana



ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS.
  DATA: WL_INPUT_CADLOT TYPE ZIMP_CAD_LOTE,
        WL_ZAA003       TYPE ZAA003.
  DATA: IT_IMPOST       TYPE TABLE OF ZIMP_LANC_IMPOST.


  MOVE: SY-MANDT             TO WL_INPUT_CADLOT-MANDT,
        WG_CADLOT-LOTE       TO  WL_INPUT_CADLOT-LOTE,
        WG_CADLOT-BUKRS      TO  WL_INPUT_CADLOT-BUKRS,
        WG_CADLOT-DESCR_LOTE TO  WL_INPUT_CADLOT-DESCR_LOTE,
        WG_CADLOT-USNAM      TO  WL_INPUT_CADLOT-USNAM,
        WG_CADLOT-DEP_RESP   TO  WL_INPUT_CADLOT-DEP_RESP,
        WG_CADLOT-DT_VENC    TO  WL_INPUT_CADLOT-DT_VENC,
        SY-UNAME             TO WL_INPUT_CADLOT-USUARIO,
        SY-DATUM             TO WL_INPUT_CADLOT-DATA_ATUAL,
        SY-UZEIT             TO WL_INPUT_CADLOT-HORA_ATUAL.

  DELETE FROM ZIMP_CAD_LOTE WHERE LOTE = WG_CADLOT-LOTE.
  MODIFY ZIMP_CAD_LOTE FROM WL_INPUT_CADLOT.

* _____ Alteração feita por Enio Jesus ______
* _____ Data 08/01/2015 _____

  SELECT SINGLE *
    FROM ZAA003
    INTO WL_ZAA003
   WHERE LOTE = WG_CADLOT-LOTE.

  IF ( SY-SUBRC IS INITIAL ).
    UPDATE ZAA003
       SET DT_VENC = WG_CADLOT-DT_VENC
     WHERE LOTE = WG_CADLOT-LOTE.
  ENDIF.
*  __________________________________________

  SELECT *
    FROM ZIMP_LANC_IMPOST
  INTO TABLE IT_IMPOST
   WHERE LOTE = WL_INPUT_CADLOT-LOTE
     AND BUKRS = WL_INPUT_CADLOT-BUKRS.

  IF IT_IMPOST[] IS NOT INITIAL.
    UPDATE ZIMP_LANC_IMPOST
       SET DT_VENC = WL_INPUT_CADLOT-DT_VENC
     WHERE LOTE = WL_INPUT_CADLOT-LOTE
       AND BUKRS = WL_INPUT_CADLOT-BUKRS.
  ENDIF.

  SET PARAMETER ID 'LOTEZIMP' FIELD WL_INPUT_CADLOT-LOTE.

  MESSAGE S836(SD) WITH 'Lote'
                         WG_CADLOT-LOTE
                         ', criado/modificado com sucesso!'.


*   " 125580-CS2023000810 Ajuste ZIMP (Data de Vencimento) - SMC - Inicio <<< - Equalização ECC x Hana
*  TRY.
*      CALL METHOD zcl_miro=>verificar_vencimento_fatura
*        EXPORTING
*          i_data_vencimento = wl_input_cadlot-dt_venc
*          i_pymt_meth       = ' '
*          i_data_se         = ' '
*          i_ck_revisao      = 'X'.
*
*    CATCH zcx_miro_exception INTO DATA(ex_miro).
*      ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*      CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO tg_msg_ret-msg SEPARATED BY space.
*    APPEND tg_msg_ret.
*    CLEAR: tg_msg_ret.
**    CATCH zcx_error INTO ex_erro.
**      ex_erro->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
**      EXIT.
*  ENDTRY.
*  " 125580-CS2023000810 Ajuste ZIMP (Data de Vencimento) - SMC - fim <<< - Equalização ECC x Hana

ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_CAMPOS .
  CLEAR: WG_CADLOT ,WG_MENSAGEM.
  MOVE SY-UNAME TO WG_CADLOT-USNAM.

ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OBTEM_PROXIMO .
  DATA: VNUM(10) TYPE C,
        VSEQ(10) TYPE P.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZID_LOT'
    IMPORTING
      NUMBER      = VSEQ.

  VNUM = VSEQ .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = VNUM
    IMPORTING
      OUTPUT = VNUM.

  WG_CADLOT-LOTE = VNUM.
ENDFORM.                    " OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_0597   text
*      -->P_C_1  text
*      -->P_C_0  text
*----------------------------------------------------------------------*
FORM TRATA_CAMPOS  USING    P_FIELD
                            P_GROUP1
                            P_VALUE
                            P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.
ENDFORM.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ TG_FIELDS-CAMPO
      OR SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.
        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
*        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ELIMINAR_LOTE .
  DATA: WL_ZIMP_CAD_LOTE TYPE ZIMP_CAD_LOTE.

  SELECT  SINGLE *
    FROM ZIMP_CAD_LOTE
    INTO WL_ZIMP_CAD_LOTE
     WHERE LOTE EQ WG_CADLOT-LOTE.

  IF SY-SUBRC IS INITIAL.
    IF WL_ZIMP_CAD_LOTE-LOEKZ IS INITIAL .
"CS2025000099 REVERSÃO TRAVAS ZIMP53 e ZIMP57 - BG #164810 - INICIO
      IF WL_ZIMP_CAD_LOTE-STATUS_LOTE NE 'A'.
        MOVE: C_X TO WL_ZIMP_CAD_LOTE-LOEKZ.
        MODIFY ZIMP_CAD_LOTE FROM WL_ZIMP_CAD_LOTE.
        MESSAGE S836(SD) WITH 'O documento foi eliminado!'.
      ELSE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o lote'
                                               'ja foi aprovado!'.
      ENDIF.
"CS2025000099 REVERSÃO TRAVAS ZIMP53 e ZIMP57 - BG #164810 - FIM
    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                                             'já foi marcado para eliminação!'.
    ENDIF.
  ENDIF.

ENDFORM.                    " ELIMINAR_LOTE
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_DEPTO INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_DEP OCCURS 0,
          DEP_RESP TYPE ZIMP_CAD_IMPOSTO-DEP_RESP,
          TEXT1    TYPE T012T-TEXT1,
        END OF TL_DEP.
  REFRESH TL_DEP.
  CLEAR TL_DEP.

  SELECT DEP_RESP DEP_RESP_DESC
    FROM ZIMP_CAD_DEPTO
    INTO TABLE TL_DEP.

*  TL_DEP-DEP_RESP = '01'.
*  TL_DEP-TEXT1    = 'Tributos Indiretos'.
*  APPEND TL_DEP.

*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '02'.
*  TL_DEP-TEXT1    = 'Tributos Diretos'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '03'.
*  TL_DEP-TEXT1    = 'Recursos Humanos'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '04'.
*  TL_DEP-TEXT1    = 'Jurídico'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '05'.
*  TL_DEP-TEXT1    = 'Patrimônio'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '06'.
*  TL_DEP-TEXT1    = 'Sustentabilidade'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '07'.
*  TL_DEP-TEXT1    = 'Tributário'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '08'.
*  TL_DEP-TEXT1    = 'AMAGGI & LDCommodities'.
*  APPEND TL_DEP.
*
*  CLEAR TL_DEP.
*  TL_DEP-DEP_RESP = '09'.
*  TL_DEP-TEXT1    = 'Contabilidade'.
*  APPEND TL_DEP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DEP_RESP'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZIMP_CAD_IMPOSTO-DEP_RESP'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_DEP
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_LOTE INPUT.
  "DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
  "           TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_LOTE OCCURS 0,
          LOTE       TYPE ZIMP_CAD_LOTE-LOTE,
          DESCR_LOTE TYPE ZIMP_CAD_LOTE-DESCR_LOTE,
          BUKRS      TYPE ZIMP_CAD_LOTE-BUKRS,
        END OF TL_LOTE.

  SELECT LOTE DESCR_LOTE BUKRS
    FROM ZIMP_CAD_LOTE
    INTO TABLE TL_LOTE
    WHERE LOEKZ = ''
    AND   STATUS_LOTE = ' '.



  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'LOTE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZIMP_CAD_LOTE-LOTE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_LOTE
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_LOTE  INPUT
