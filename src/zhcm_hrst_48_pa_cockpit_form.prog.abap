*----------------------------------------------------------------------*
*  Módulo    : HCM                                                     *
*  Programa  : ZHCM_HRST_48_PA_COCKPIT_FORM                            *
*  Autor     : Aceleradores HRST                                       *
*  Descrição : Medidas Disciplinares.       *
*  Obs.      :                                                         *
*----------------------------------------------------------------------*

REPORT zhcm_hrst_48_pa_cockpit_form.



*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: pa0002,
        pa0001,
        pernr.

DATA: tg_zhcmcsc002 TYPE TABLE OF zhcmcsc002,
      tg_rsparams   TYPE TABLE OF rsparams.

DATA: it_tsp01 TYPE STANDARD TABLE OF tsp01.
*----------------------------------------------------------------------*
* Estruturas
*----------------------------------------------------------------------*
DATA: wg_zhcmcsc002 TYPE zhcmcsc002,
      wg_or_objec   TYPE objec,
      wg_rsparams   TYPE rsparams.
*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: vg_first(1) TYPE c,
      vg_varname  TYPE raldb_vari.

DATA: vl_imprimiu TYPE char1,
      mc_todos    TYPE char1.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: c_pf(3) TYPE c VALUE 'PF_',
           c_0(1)  TYPE c VALUE '0',
           c_1(1)  TYPE c VALUE '1',
           c_x(1)  TYPE c VALUE 'X',
           c_e(1)  TYPE c VALUE 'E'.

*&---------------------------------------------------------------------*
*& Macros
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
* Selecionar dados do cockpit
  PERFORM select_dados_cockpit.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK 9 WITH FRAME TITLE text-004.
  PARAMETERS    : p_cargo     TYPE c            AS CHECKBOX USER-COMMAND imediato.
  SELECT-OPTIONS: s_cargo     FOR  pa0001-stell NO INTERVALS,
                  s_dtadm     FOR  sy-datum.
  SELECTION-SCREEN   END OF BLOCK 9.

  SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-003.
  PARAMETERS: p_print  TYPE c         AS CHECKBOX USER-COMMAND imediato,
              p_pdest  TYPE rspopname MATCHCODE OBJECT prin,
              p_alltog TYPE c AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN   END OF BLOCK 1.


  SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-002.
* Parametros Dinâmicos
*** ATENÇÃO - NAO PODE CRIAR OUTROS PARAMETROS QUE TENHA O NOME DE PF_ ***
  PARAMETERS: p_td AS CHECKBOX USER-COMMAND imediato. "Marca todos os checkbox
  PARAMETERS:
*    PF_01 AS CHECKBOX, PF_02     AS CHECKBOX, PF_03     AS CHECKBOX, PF_24 AS CHECKBOX,
*    PF_04 AS CHECKBOX USER-COMMAND IMEDIATO,
*    PF_05 AS CHECKBOX USER-COMMAND IMEDIATO, PF_06     AS CHECKBOX USER-COMMAND IMEDIATO, PF_07     AS CHECKBOX, PF_08     AS CHECKBOX USER-COMMAND IMEDIATO,
*    PF_09 AS CHECKBOX USER-COMMAND IMEDIATO, PF_10     AS CHECKBOX USER-COMMAND IMEDIATO, PF_11     AS CHECKBOX, PF_12     AS CHECKBOX USER-COMMAND IMEDIATO,
*    PF_13 AS CHECKBOX USER-COMMAND IMEDIATO, PF_14     AS CHECKBOX USER-COMMAND IMEDIATO, PF_15     AS CHECKBOX, PF_16     AS CHECKBOX USER-COMMAND IMEDIATO,
*    PF_17 AS CHECKBOX USER-COMMAND IMEDIATO, PF_18     AS CHECKBOX USER-COMMAND IMEDIATO, PF_19     AS CHECKBOX, PF_20     AS CHECKBOX USER-COMMAND IMEDIATO,
*    PF_21 AS CHECKBOX USER-COMMAND IMEDIATO, " pf_24 opção Marcar todos,
*    PF_22 AS CHECKBOX USER-COMMAND IMEDIATO, PF_23     AS CHECKBOX USER-COMMAND IMEDIATO.
    pf_01 AS CHECKBOX USER-COMMAND imediato,
    pf_02 AS CHECKBOX USER-COMMAND imediato.
    "pf_03 AS CHECKBOX USER-COMMAND imediato,
    "pf_04 AS CHECKBOX USER-COMMAND imediato,
    "pf_05 AS CHECKBOX USER-COMMAND imediato,
    "pf_06 AS CHECKBOX USER-COMMAND imediato,
    "pf_07 AS CHECKBOX USER-COMMAND imediato,
    "pf_08 AS CHECKBOX USER-COMMAND imediato,
    "pf_09 AS CHECKBOX USER-COMMAND imediato,
    "pf_10 AS CHECKBOX USER-COMMAND imediato,
    "pf_11 AS CHECKBOX USER-COMMAND imediato,
    "pf_12 AS CHECKBOX USER-COMMAND imediato,
    "pf_13 AS CHECKBOX USER-COMMAND imediato,
    "pf_14 AS CHECKBOX USER-COMMAND imediato,
*    pf_15 AS CHECKBOX USER-COMMAND imediato,
    "pf_16 AS CHECKBOX USER-COMMAND imediato,
    "pf_17 AS CHECKBOX USER-COMMAND imediato. "*** - PBI - 72092 - CBRAND


  "Para criar novas opções no cockpit tirar o comentario abaixo . Cada Checkbox será uma opção.
  "Cadastrar o numero da opção no programa  ZHCMR_BN0018

*    PF_25 AS CHECKBOX, PF_26     AS CHECKBOX, PF_27     AS CHECKBOX, PF_28     AS CHECKBOX,
*    PF_29 AS CHECKBOX, PF_30     AS CHECKBOX, PF_31     AS CHECKBOX, PF_32     AS CHECKBOX,
*    PF_33 AS CHECKBOX, PF_34     AS CHECKBOX, PF_35     AS CHECKBOX, PF_36     AS CHECKBOX,
*    PF_37 AS CHECKBOX, PF_38     AS CHECKBOX, PF_39     AS CHECKBOX, PF_40     AS CHECKBOX,
*    PF_41 AS CHECKBOX, PF_42     AS CHECKBOX, PF_43     AS CHECKBOX, PF_44     AS CHECKBOX,
*    PF_45 AS CHECKBOX, PF_46     AS CHECKBOX, PF_47     AS CHECKBOX, PF_48     AS CHECKBOX,
*    PF_49 AS CHECKBOX, PF_50     AS CHECKBOX, PF_51     AS CHECKBOX, PF_52     AS CHECKBOX,
*    PF_53 AS CHECKBOX, PF_54     AS CHECKBOX, PF_55     AS CHECKBOX, PF_56     AS CHECKBOX,
*    PF_57 AS CHECKBOX, PF_58     AS CHECKBOX, PF_59     AS CHECKBOX, PF_60     AS CHECKBOX,
*    PF_61 AS CHECKBOX, PF_62     AS CHECKBOX, PF_63     AS CHECKBOX, PF_64     AS CHECKBOX,
*    PF_65 AS CHECKBOX, PF_66     AS CHECKBOX, PF_67     AS CHECKBOX, PF_68     AS CHECKBOX,
*    PF_69 AS CHECKBOX, PF_70     AS CHECKBOX, PF_71     AS CHECKBOX, PF_72     AS CHECKBOX,
*    PF_73 AS CHECKBOX, PF_74     AS CHECKBOX, PF_75     AS CHECKBOX, PF_76     AS CHECKBOX,
*    PF_77 AS CHECKBOX, PF_78     AS CHECKBOX, PF_79     AS CHECKBOX, PF_80     AS CHECKBOX,
*    PF_81 AS CHECKBOX, PF_82     AS CHECKBOX, PF_83     AS CHECKBOX, PF_84     AS CHECKBOX,
*    PF_85 AS CHECKBOX, PF_86     AS CHECKBOX, PF_87     AS CHECKBOX, PF_88     AS CHECKBOX,
*    PF_89 AS CHECKBOX, PF_90     AS CHECKBOX, PF_91     AS CHECKBOX, PF_92     AS CHECKBOX,
*    PF_93 AS CHECKBOX, PF_94     AS CHECKBOX, PF_95     AS CHECKBOX, PF_96     AS CHECKBOX,
*    PF_97 AS CHECKBOX, PF_98     AS CHECKBOX, PF_99     AS CHECKBOX.
  SELECTION-SCREEN   END OF BLOCK 2.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

* Montar tela do cockpit
  PERFORM montar_tela_cockpit.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON.
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_print.
* Montar tela do cockpit
  PERFORM mudar_p_print.

AT SELECTION-SCREEN ON p_td.
  PERFORM marcar_pf.

AT SELECTION-SCREEN ON pf_01.
  PERFORM marcar_pf.

AT SELECTION-SCREEN ON pf_02.
  PERFORM marcar_pf.

*AT SELECTION-SCREEN ON pf_03.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_04.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_05.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_06.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_07.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_08.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_09.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_10.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_11.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_12.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_13.
*  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_14.
*  PERFORM marcar_pf.
*
**AT SELECTION-SCREEN ON pf_15.
**  PERFORM marcar_pf.
*
*AT SELECTION-SCREEN ON pf_16.
*  PERFORM marcar_pf.
**** - PBI - 72092 - Inicio - CBRAND
*AT SELECTION-SCREEN ON pf_17.
*  PERFORM marcar_pf.
**** - PBI - 72092 - Fim - CBRAND


*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM check_dispositivo_saida.

  IF NOT p_cargo IS INITIAL.
* Selecionar funcionários por Cargo e Data de admissão
    PERFORM select_fun_cargo_dtadm.

  ENDIF.

  IF pnppernr[] IS INITIAL.
* Informar ao menos um funcionário!!!
    MESSAGE i013(zhcm) DISPLAY LIKE c_e.
    LEAVE TO CURRENT TRANSACTION.
  ELSEIF pnpbukrs[] IS NOT INITIAL.
    SELECT COUNT( * ) INTO @DATA(lv_cont)
      FROM pa0001
      WHERE pernr IN @pnppernr[]
    AND bukrs IN @pnpbukrs[].
    IF lv_cont IS INITIAL.
      MESSAGE i038(zhcm) DISPLAY LIKE c_e.
      LEAVE TO CURRENT TRANSACTION.
    ENDIF.
  ENDIF.

* Chamar formulários
  PERFORM chamar_formularios.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DADOS_COCKPIT
*&---------------------------------------------------------------------*
*  Selecionar dados do COCKPIT
*----------------------------------------------------------------------*
FORM select_dados_cockpit .

* Selecionar configurações do COCKPIT
  REFRESH: tg_zhcmcsc002.
  SELECT *
  FROM zhcmcsc002
  INTO CORRESPONDING FIELDS OF TABLE tg_zhcmcsc002
  WHERE prgcock EQ sy-repid ORDER BY PRIMARY KEY .
  IF sy-subrc NE 0.
    MESSAGE e008(zhcm) WITH sy-repid.
    "ELSE.
    "SORT TG_ZHCMCSC002 BY NROFORM.
  ENDIF.

ENDFORM.                    " SELECT_DADOS_COCKPIT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_TELA_COCKPIT
*&---------------------------------------------------------------------*
*  Montar tela dinâmica do Cockpit
*----------------------------------------------------------------------*
FORM montar_tela_cockpit.

  DATA: vl_text(50) TYPE c,
        vl_nroform  TYPE zhcmcs_ed_nroform.

  FIELD-SYMBOLS: <fs_texto> TYPE any.

  LOOP AT SCREEN.
* Verificar se o campo é um parametro de formulário
    IF screen-name(3) EQ c_pf.

      CLEAR: vl_nroform,
             vl_text.

* Pegar nro do parametro
      vl_nroform = screen-name+3.

* Verificar se o parametro está cadastrado
      CLEAR: wg_zhcmcsc002.
      READ TABLE tg_zhcmcsc002 INTO wg_zhcmcsc002 WITH KEY nroform = vl_nroform
                               BINARY SEARCH.
      IF sy-subrc EQ 0.
* Habilitar parametro
        screen-invisible = c_0.
        screen-active    = c_1.
        screen-input     = c_1.

* Adicionar texto no parametro
        CONCATENATE '%_'
                    screen-name
                    '_%_APP_%-TEXT'
                    INTO vl_text.

        ASSIGN (vl_text) TO <fs_texto>.
        IF sy-subrc EQ 0.
          MOVE: wg_zhcmcsc002-nomform TO <fs_texto>.
          UNASSIGN <fs_texto>.
        ELSE.
        ENDIF.

** Pré-selecionar formulário
*        IF NOT wg_zhcmcsc002-fpadrao IS INITIAL AND vg_first IS INITIAL.
*          ASSIGN (screen-name) TO <fs_texto>.
*          IF sy-subrc EQ 0.
*            MOVE: c_x TO <fs_texto>.
*            UNASSIGN <fs_texto>.
*          ELSE.
*          ENDIF.
*        ENDIF.

      ELSE.
* Ocultar parametro
        screen-invisible = c_1 .
        screen-active    = c_0 .
        screen-input     = c_0.
      ENDIF.

    ENDIF. "IF screen-name(3) EQ c_pf.

    IF screen-name EQ 'P_PDEST'.
      IF p_print IS INITIAL.
        CLEAR: p_pdest.
        screen-input     = c_0.
      ELSE.
        screen-input     = c_1.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'S_DTADM-HIGH' OR
       screen-name EQ 'S_DTADM-LOW'  OR
       screen-name EQ 'S_CARGO-LOW'.
      IF p_cargo IS INITIAL.
        CLEAR: s_dtadm,
               s_cargo.
        REFRESH: s_dtadm,
                 s_cargo.
        screen-input     = c_0.
      ELSE.
        screen-input     = c_1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

  vg_first = c_x.

ENDFORM.                    " MONTAR_TELA_COCKPIT
*&---------------------------------------------------------------------*
*&      Form  CHAMAR_FORMULÁRIOS
*&---------------------------------------------------------------------*
*  Chamar formulários marcados
*----------------------------------------------------------------------*
FORM chamar_formularios .

  DATA: vl_text(40)  TYPE c,
        vl_text2(40) TYPE c,
        vl_text1(50) TYPE c,
        vl_subrc     TYPE sy-subrc,
        vl_imp(1)    TYPE c,
        vl_time      TYPE i,
        vl_cont      TYPE i.

  FIELD-SYMBOLS: <fs_param> TYPE any.

  CLEAR: vl_imp, vl_imprimiu.

  PERFORM criar_variant.

  CLEAR vl_cont.


  IF pf_01 = 'X' AND  pf_02  = 'X'. "AND pf_03  = 'X' AND  pf_04  = 'X' AND
"     pf_05 = 'X' AND  pf_06  = 'X' AND pf_07  = 'X' AND  pf_08  = 'X' AND
"     pf_09 = 'X' AND  pf_10  = 'X' AND pf_11  = 'X' AND  pf_12  = 'X' AND
"     pf_13 = 'X' AND  pf_14  = 'X' AND pf_16  = 'X' AND  pf_17  = 'X'.
*    AND pf_15 = 'X' .
    SORT tg_zhcmcsc002 BY seq_impressao.
  ENDIF.


  LOOP AT tg_zhcmcsc002 INTO wg_zhcmcsc002.

    CLEAR: vl_text.

    CONCATENATE c_pf
                wg_zhcmcsc002-nroform
                INTO vl_text.

    ASSIGN (vl_text) TO <fs_param>.

    "IF PF_22 EQ ABAP_TRUE. "User marcou impressão total
*    IF PF_24 EQ ABAP_TRUE. "User marcou impressão total
*      ASSIGN 'X' TO <FS_PARAM>.
*    ENDIF.

    CHECK sy-subrc EQ 0.

* Verificar se o formulário está marcado para impressão
    IF NOT <fs_param> IS INITIAL.
* Verificar se o Nome do programa do formulário está preenchido
      PERFORM check_progform CHANGING vl_subrc.

      CHECK vl_subrc IS INITIAL.

* Se flag imprimir duas vias estiver marcado, chamar o formulário duas vezes
      CLEAR: vl_time.
      IF NOT wg_zhcmcsc002-duasvia IS INITIAL.
        vl_time = 2.
      ELSE.
        vl_time = 1.
      ENDIF.

      DO vl_time TIMES.
        PERFORM submit_formulario.
        IF wg_zhcmcsc002-prgform = 'ZHCMR_BN0001'.
          DATA(v_check_prog) = 'ZHCMR_BN0001'.
        ENDIF.
      ENDDO.

      ADD 1 TO vl_cont.
*      IF VL_TEXT EQ 'PF_16'.
*        VL_TEXT2 = 'PF_16'.
*      ENDIF.

*      IF VL_TEXT EQ 'PF_10'.
*        VL_TEXT2 = 'PF_10'.
*      ENDIF.

      IF p_alltog EQ abap_false.
        PERFORM print_all_together CHANGING vl_imprimiu.
        IF vl_imprimiu EQ abap_true.
          vl_imp = c_x.
        ELSE.
          vl_imp = abap_false.
        ENDIF.
      ELSE.
        vl_imp = c_x.
      ENDIF.

    ENDIF. "IF NOT <fs_param> IS INITIAL.

  ENDLOOP.

  "IF VL_TEXT2 EQ 'PF_16' AND VL_CONT = 1 . "Seguro em grupo Gera no diretorio SEGURO quando gerado individualmente
  "IF VL_TEXT2 EQ 'PF_10' AND VL_CONT = 1 . "Seguro em grupo Gera no diretorio SEGURO quando gerado individualmente
  IF v_check_prog = 'ZHCMR_BN0001' AND vl_cont = 1.
    MESSAGE 'Seguro em grupo gerado no diretorio C:\SEGURO\' TYPE 'I'.
  ELSE.
    PERFORM pagar_variant.

    IF NOT vl_imp IS INITIAL.
      IF p_alltog EQ abap_true.
        PERFORM print_all_together CHANGING vl_imprimiu.
        IF vl_imprimiu IS NOT INITIAL.
          MESSAGE s010(zhcm).
        ELSE.
          MESSAGE i000(zhcm) WITH 'Não foi possível imprimir Formulário' DISPLAY LIKE c_e.
        ENDIF.
      ELSE.
        MESSAGE s010(zhcm).
      ENDIF.
    ELSE.
      MESSAGE i011(zhcm) DISPLAY LIKE c_e.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHAMAR_FORMULÁRIOS
*&---------------------------------------------------------------------*
*&      Form  CHECK_PROGFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_SUBRC  text
*----------------------------------------------------------------------*
FORM check_progform  CHANGING pf_subrc.

  DATA: vl_text1(50)    TYPE c.

  CLEAR: pf_subrc.

  IF wg_zhcmcsc002-prgform IS INITIAL.

    CONCATENATE wg_zhcmcsc002-prgcock
                '/'
                wg_zhcmcsc002-nroform
                INTO vl_text1 SEPARATED BY space.

    MESSAGE i009(zhcm) WITH 'PRGFORM'
                          vl_text1
                     DISPLAY LIKE c_e.

    pf_subrc = 4.

  ENDIF.

ENDFORM.                    " CHECK_PROGFORM
*&---------------------------------------------------------------------*
*&      Form  submit_formulario
*&---------------------------------------------------------------------*
*  Chamada do Formulário
*----------------------------------------------------------------------*
FORM submit_formulario.

  DATA: lt_param TYPE TABLE OF rsparams,
        ls_param TYPE rsparams.

****************

  DATA: it_params LIKE pri_params,
        vl_chave  TYPE char30.

  CONCATENATE sy-datum sy-uzeit sy-uname sy-mandt INTO vl_chave.

  "Busca parâmetros atuais de impressão
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      mode                   = 'CURRENT'
      no_dialog              = 'X'
    IMPORTING
      out_parameters         = it_params
    EXCEPTIONS
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      OTHERS                 = 4.

  "Marca o parâmetro encontrado com VL_CHAVE e seta a variável PRIMM para que não imprima a ordem Spool imediatamente
  it_params-prtxt = vl_chave.
  it_params-primm = abap_false.
  it_params-armod = 1.
****************

  IF  wg_zhcmcsc002-ztipo_prog = 'P_ALL'.


*    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*      EXPORTING
*        FORMNAME                 =
**       VARIANT                  = ' '
**       DIRECT_CALL              = ' '
**     IMPORTING
**       FM_NAME                  =
**     EXCEPTIONS
**       NO_FORM                  = 1
**       NO_FUNCTION_MODULE       = 2
**       OTHERS                   = 3
*              .
*    IF SY-SUBRC <> 0.
** Implement suitable error handling here
*    ENDIF.




*    DATA: PRINT_PARAMETERS TYPE PRI_PARAMS,
*          VALID_FLAG(2)    TYPE C.
*
*    CALL FUNCTION 'GET_PRINT_PARAMETERS'
*      IMPORTING
*        OUT_PARAMETERS       = PRINT_PARAMETERS
*        VALID                = VALID_FLAG
*      EXCEPTIONS
*        INVALID_PRINT_PARAMS = 2
*        OTHERS               = 4.
*
*    CHECK SY-SUBRC = 0 AND VALID_FLAG = SPACE.
*
*    NEW-PAGE PRINT ON PARAMETERS PRINT_PARAMETERS
*
*    NO DIALOG.
*
*    WRITE:/ '1'.
*
*    NEW-PAGE PRINT OFF.
*
*    NEW-PAGE PRINT ON PARAMETERS PRINT_PARAMETERS
*
*    NO DIALOG.
*
*    WRITE:/ '2'.
*
*    NEW-PAGE PRINT OFF.
*
*    NEW-PAGE PRINT ON PARAMETERS PRINT_PARAMETERS
*
*    NO DIALOG.
*
*    WRITE:/ '3'.

**    NEW-PAGE PRINT OFF.

  ELSE.

    REFRESH lt_param.

    ls_param-selname = 'P_CHAVE'.
    ls_param-kind    = 'P'.
    ls_param-sign    = 'I'.
    ls_param-option  = 'EQ'.
    ls_param-low     = vl_chave.
    APPEND ls_param TO lt_param.
    CLEAR: ls_param.
*
*    LS_PARAM-SELNAME = 'GETOTF'.
*    LS_PARAM-KIND    = 'P'.
*    LS_PARAM-SIGN    = 'I'.
*    LS_PARAM-OPTION  = 'EQ'.
*    LS_PARAM-LOW     = 'X'.
*    APPEND LS_PARAM TO LT_PARAM.
*    CLEAR: LS_PARAM.
*
*    LS_PARAM-SELNAME = 'DIALOG'.
*    LS_PARAM-KIND    = 'P'.
*    LS_PARAM-SIGN    = 'I'.
*    LS_PARAM-OPTION  = 'EQ'.
*    LS_PARAM-LOW     = 'X'.
*    APPEND LS_PARAM TO LT_PARAM.
*    CLEAR: LS_PARAM.
*
*    LS_PARAM-SELNAME = 'PREVIEW'.
*    LS_PARAM-KIND    = 'P'.
*    LS_PARAM-SIGN    = 'I'.
*    LS_PARAM-OPTION  = 'EQ'.
*    LS_PARAM-LOW     = ' '.
*    APPEND LS_PARAM TO LT_PARAM.
*    CLEAR: LS_PARAM.

    IF wg_zhcmcsc002-ztipo_prog IS NOT INITIAL.

      FIELD-SYMBOLS <fs_tp_prog> TYPE char10.
      DATA lv_tp_prog TYPE char10.

*      DATA: LT_PARAM TYPE TABLE OF RSPARAMS,
*            LS_PARAM TYPE RSPARAMS.

      lv_tp_prog = wg_zhcmcsc002-ztipo_prog.
      ASSIGN (lv_tp_prog) TO <fs_tp_prog>.

*      REFRESH LT_PARAM.

      ls_param-selname = wg_zhcmcsc002-ztipo_prog.
      ls_param-kind    = 'P'.
      ls_param-sign    = 'I'.
      ls_param-option  = 'EQ'.
      ls_param-low    = 'X'.
      APPEND ls_param TO lt_param.
      CLEAR: ls_param.

* Chamar Formulário Dinâmico
      SUBMIT (wg_zhcmcsc002-prgform)
********** INÍCIO Manda para o Submit os parâmetros de impressão setados no início do formulário
      TO SAP-SPOOL
      WITHOUT SPOOL DYNPRO
      SPOOL PARAMETERS it_params
********** FIM
            WITH SELECTION-TABLE lt_param
            USING SELECTION-SET vg_varname
            USING SELECTION-SETS OF PROGRAM sy-repid
            AND RETURN.

    ELSE.
* Chamar Formulário Dinâmico
      SUBMIT (wg_zhcmcsc002-prgform)
********** INÍCIO Manda para o Submit os parâmetros de impressão setados no início do formulário
      TO SAP-SPOOL
      WITHOUT SPOOL DYNPRO
      SPOOL PARAMETERS it_params
      WITH SELECTION-TABLE lt_param
********** FIM
            USING SELECTION-SET vg_varname
            USING SELECTION-SETS OF PROGRAM sy-repid
             AND RETURN.
    ENDIF.
  ENDIF.

********** INÍCIO Captura tudo o que foi produzido de ordem Spool no Submit.
********** Se é impressão de todos os formulários, não apaga a tabela IT_TSP01.
  COMMIT WORK.

  IF p_alltog EQ abap_false.
    CLEAR: it_tsp01.
  ENDIF.

  SELECT *
  FROM tsp01
  APPENDING TABLE it_tsp01
  WHERE rqowner EQ sy-uname
  AND rqtitle EQ vl_chave
  ORDER BY rqident ASCENDING.
********** FIM

ENDFORM.                                                    " submit_formulario
*&---------------------------------------------------------------------*
*&      Form  CRIAR_VARIANT
*&---------------------------------------------------------------------*
*  Função para criar variant dinamica em runtime
*----------------------------------------------------------------------*
FORM criar_variant.

  DATA: wl_vardesc TYPE          varid,
        tl_vartext TYPE TABLE OF varit,
        wl_vartext TYPE          varit.

  REFRESH: tg_rsparams.

  PERFORM parameters.

  PERFORM select_options.

* Nome da variante
  CONCATENATE sy-datum sy-uzeit
  INTO vg_varname.

* Descrição da variante
  CLEAR wl_vardesc.
  wl_vardesc-mandt      = sy-mandt.
  wl_vardesc-report     = sy-repid.
  wl_vardesc-variant    = vg_varname.
  wl_vardesc-flag1      = space.
  wl_vardesc-flag2      = space.
  wl_vardesc-transport  = space.
  wl_vardesc-environmnt = 'A'.       "Variante gilt für Batch und Dial
  wl_vardesc-protected  = space.
  wl_vardesc-secu       = space.
  wl_vardesc-version    = '1'.
  wl_vardesc-ename      = sy-uname.
  wl_vardesc-edat       = sy-datum.
  wl_vardesc-etime      = sy-uzeit.
  wl_vardesc-aename     = space.
  wl_vardesc-aedat      = space.
  wl_vardesc-aetime     = space.
  wl_vardesc-mlangu     = sy-langu.

* Variante text
  CLEAR: wl_vartext.
  REFRESH: tl_vartext.

  wl_vartext-mandt      = sy-mandt.
  wl_vartext-langu      = sy-langu.
  wl_vartext-report     = sy-repid.
  wl_vartext-variant    = vg_varname.
  wl_vartext-vtext      = ' Variante ZHCMCSR105'.
  APPEND wl_vartext TO tl_vartext.

  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      curr_report               = sy-repid
      curr_variant              = vg_varname
      vari_desc                 = wl_vardesc
    TABLES
      vari_contents             = tg_rsparams
      vari_text                 = tl_vartext
*     VSCREENS                  =
    EXCEPTIONS
      illegal_report_or_variant = 1
      illegal_variantname       = 2
      not_authorized            = 3
      not_executed              = 4
      report_not_existent       = 5
      report_not_supplied       = 6
      variant_exists            = 7
      variant_locked            = 8
      OTHERS                    = 9.
  IF sy-subrc NE 0.
    MESSAGE e398(00) WITH 'Erro ao criar variante!!!'.
  ENDIF.

ENDFORM.                    " CRIAR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_PARAMETER
*&---------------------------------------------------------------------*
*   Preencher tabelas de parameters
*----------------------------------------------------------------------*
FORM preencher_parameter  USING    p_nmparam
                                   p_vlparam.

  CHECK NOT p_vlparam IS INITIAL.

  CLEAR: wg_rsparams.
  wg_rsparams-selname = p_nmparam.
  wg_rsparams-kind    = 'P'.
  wg_rsparams-option  = 'I'.
  wg_rsparams-sign    = 'EQ'.
  wg_rsparams-low     = p_vlparam.

  APPEND wg_rsparams TO tg_rsparams.

ENDFORM.                    " PREENCHER_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  PARAMETERS
*&---------------------------------------------------------------------*
*  Preencher parameters
*----------------------------------------------------------------------*
FORM parameters .

  IF NOT p_print IS INITIAL.
    PERFORM preencher_parameter USING 'P_PDEST'
                                      p_pdest.
  ENDIF.

  PERFORM preencher_parameter USING 'P_PRINT'
                                    c_x.

  PERFORM preencher_parameter USING 'PYBEGDA'
                                    pybegda.

  PERFORM preencher_parameter USING 'PYENDDA'
                                    pyendda.

  PERFORM preencher_parameter USING 'PYVW0_0'
                                    pyvw0_0.

  PERFORM preencher_parameter USING 'PYVW1_0'
                                    pyvw1_0.

  PERFORM preencher_parameter USING 'PYVW2_0'
                                    pyvw2_0.

  PERFORM preencher_parameter USING 'PYXABKR'
                                    pyxabkr.

  PERFORM preencher_parameter USING 'PYPA03R0'
                                    pypa03r0.

  PERFORM preencher_parameter USING 'PYPA03R1'
                                    pypa03r1.

  PERFORM preencher_parameter USING 'PYABRP0'
                                    pyabrp0.

  PERFORM preencher_parameter USING 'PYABRJ0'
                                    pyabrj0.

  PERFORM preencher_parameter USING 'PYABRP1'
                                    pyabrp1.

  PERFORM preencher_parameter USING 'PYABRJ1'
                                    pyabrj1.

  PERFORM preencher_parameter USING 'PYVW0_1'
                                    pyvw0_1.

  PERFORM preencher_parameter USING 'PYVW1_1'
                                    pyvw1_1.

  PERFORM preencher_parameter USING 'PNPTIMR1'
                                    pnptimr1.

  PERFORM preencher_parameter USING 'PNPTIMR2'
                                    pnptimr2.

  PERFORM preencher_parameter USING 'PNPTIMR3'
                                    pnptimr3.

  PERFORM preencher_parameter USING 'PNPTIMR4'
                                    pnptimr4.

  PERFORM preencher_parameter USING 'PNPTIMR5'
                                    pnptimr5.

  PERFORM preencher_parameter USING 'PNPTIMR6'
                                    pnptimr6.

  PERFORM preencher_parameter USING 'PNPBEGDA'
                                    pnpbegda.

  PERFORM preencher_parameter USING 'PNPENDDA'
                                    pnpendda.

  PERFORM preencher_parameter USING 'PNPBEGPS'
                                    pnpbegps.

  PERFORM preencher_parameter USING 'PNPENDPS'
                                    pnpendps.

  PERFORM preencher_parameter USING 'PNPXABKR'
                                    pnpxabkr.

  PERFORM preencher_parameter USING 'PNPDISBD'
                                    pnpdisbd.

  PERFORM preencher_parameter USING 'PNPDISED'
                                    pnpdised.

  PERFORM preencher_parameter USING 'PNPTIMR9'
                                    pnptimr9.

  PERFORM preencher_parameter USING 'PNPDISPP'
                                    pnpdispp.

  PERFORM preencher_parameter USING 'PNPDISPJ'
                                    pnpdispj.

  PERFORM preencher_parameter USING 'PNPTIMRA'
                                    pnptimra.

  PERFORM preencher_parameter USING 'PNPPABRP'
                                    pnppabrp.

  PERFORM preencher_parameter USING 'PNPPABRJ'
                                    pnppabrj.

  PERFORM preencher_parameter USING 'PYEVAL'
                                    pyeval.

  PERFORM preencher_parameter USING 'PYVW0'
                                    pyvw0.

  PERFORM preencher_parameter USING 'PYVW1'
                                    pyvw1.

  PERFORM preencher_parameter USING 'PYVW2'
                                    pyvw2.

  PERFORM preencher_parameter USING 'PYSCREEN'
                                    pyscreen.

  PERFORM preencher_parameter USING 'PYPARAID'
                                    pyparaid.

*  PERFORM preencher_parameter USING 'PYSORT'
*                                    pysort.
*
*  PERFORM preencher_parameter USING 'PYSORTT'
*                                    pysortt.

*  PERFORM preencher_parameter USING 'PYORGCUR'
*                                    pyorgcur.

  PERFORM preencher_parameter USING 'PYCT_PER'
                                    pyct_per.

  PERFORM preencher_parameter USING 'PYARCH'
                                    pyarch.

  PERFORM preencher_parameter USING 'PNPTIMED'
                                    pnptimed.

  PERFORM preencher_parameter USING 'PNPMCIDE'
                                    pnpmcide.

  PERFORM preencher_parameter USING 'PNPMCSTR'
                                    pnpmcstr.

  PERFORM preencher_parameter USING 'PNPSORTF'
                                    pnpsortf.

  PERFORM preencher_parameter USING 'PNPSORTT'
                                    pnpsortt.

*  PERFORM preencher_parameter USING 'PNPDYNSE'
*                                    pnpdynse.

*  PERFORM preencher_parameter USING 'PNPSELTA'
*                                    pnpselta.

*  PERFORM preencher_parameter USING 'PNPSORT'
*                                    pnpsort.

*  PERFORM preencher_parameter USING 'PNPSELOP'
*                                    pnpselop.

  PERFORM preencher_parameter USING 'PNPPLVAR'
                                    pnpplvar.

  PERFORM preencher_parameter USING 'PNPESSCF'
                                    pnpesscf.

ENDFORM.                    " PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  SELECT_OPTIONS
*&---------------------------------------------------------------------*
*   Preencher select options
*----------------------------------------------------------------------*
FORM select_options .

  PERFORM preencher_select_option TABLES pnppernr[]
                                  USING  'PNPPERNR'.

  PERFORM preencher_select_option TABLES pnpmassn[]
                                  USING  'PNPMASSN'.

  PERFORM preencher_select_option TABLES pnpmassg[]
                                  USING  'PNPMASSG'.

  PERFORM preencher_select_option TABLES pnpstat1[]
                                  USING  'PNPSTAT1'.

  PERFORM preencher_select_option TABLES pnpstat2[]
                                  USING  'PNPSTAT2'.

  PERFORM preencher_select_option TABLES pnpstat3[]
                                  USING  'PNPSTAT3'.

  PERFORM preencher_select_option TABLES pnpbukrs[]
                                  USING  'PNPBUKRS'.

  PERFORM preencher_select_option TABLES pnpwerks[]
                                  USING  'PNPWERKS'.

  PERFORM preencher_select_option TABLES pnpbtrtl[]
                                  USING  'PNPBTRTL'.

  PERFORM preencher_select_option TABLES pnppersg[]
                                  USING  'PNPPERSG'.

  PERFORM preencher_select_option TABLES pnppersk[]
                                  USING  'PNPPERSK'.

  PERFORM preencher_select_option TABLES pnpvdsk1[]
                                  USING  'PNPVDSK1'.

  PERFORM preencher_select_option TABLES pnpgsber[]
                                  USING  'PNPGSBER'.

  PERFORM preencher_select_option TABLES pnpjuper[]
                                  USING  'PNPJUPER'.

  PERFORM preencher_select_option TABLES pnpabkrs[]
                                  USING  'PNPABKRS'.

  PERFORM preencher_select_option TABLES pnpansvh[]
                                  USING  'PNPANSVH'.

  PERFORM preencher_select_option TABLES pnpkokrs[]
                                  USING  'PNPKOKRS'.

  PERFORM preencher_select_option TABLES pnpkostl[]
                                  USING  'PNPKOSTL'.

  PERFORM preencher_select_option TABLES pnporgeh[]
                                  USING  'PNPORGEH'.

  PERFORM preencher_select_option TABLES pnpplans[]
                                  USING  'PNPPLANS'.

  PERFORM preencher_select_option TABLES pnpstell[]
                                  USING  'PNPSTELL'.

  PERFORM preencher_select_option TABLES pnpmstbr[]
                                  USING  'PNPMSTBR'.

  PERFORM preencher_select_option TABLES pnpsbmod[]
                                  USING  'PNPSBMOD'.

  PERFORM preencher_select_option TABLES pnpsacha[]
                                  USING  'PNPSACHA'.

  PERFORM preencher_select_option TABLES pnpsachp[]
                                  USING  'PNPSACHP'.

  PERFORM preencher_select_option TABLES pnpsachz[]
                                  USING  'PNPSACHZ'.

  PERFORM preencher_select_option TABLES pnpotype[]
                                  USING  'PNPOTYPE'.

  PERFORM preencher_select_option TABLES pnpename[]
                                  USING  'PNPENAME'.

  PERFORM preencher_select_option TABLES pnpsname[]
                                  USING  'PNPSNAME'.

  PERFORM preencher_select_option TABLES pnpfistl[]
                                  USING  'PNPFISTL'.

  PERFORM preencher_select_option TABLES pnpgeber[]
                                  USING  'PNPGEBER'.

  PERFORM preencher_select_option TABLES pnpfkber[]
                                  USING  'PNPFKBER'.

  PERFORM preencher_select_option TABLES pnpgrant[]
                                  USING  'PNPGRANT'.

  PERFORM preencher_select_option TABLES pnpmasng[]
                                  USING  'PNPMASNG'.

  PERFORM preencher_select_option TABLES pnpstatu[]
                                  USING  'PNPSTATU'.

  PERFORM preencher_select_option TABLES pnpxbwbk[]
                                  USING  'PNPXBWBK'.

  PERFORM preencher_select_option TABLES pnpkoktl[]
                                  USING  'PNPKOKTL'.

  PERFORM preencher_select_option TABLES pnpxpgpk[]
                                  USING  'PNPXPGPK'.

  PERFORM preencher_select_option TABLES pnpsasba[]
                                  USING  'PNPSASBA'.

  PERFORM preencher_select_option TABLES pnpsasbp[]
                                  USING  'PNPSASBP'.

  PERFORM preencher_select_option TABLES pnpsasbz[]
                                  USING  'PNPSASBZ'.

  PERFORM preencher_select_option TABLES pnpindex[]
                                  USING  'PNPINDEX'.

  PERFORM preencher_select_option TABLES pnpobjid[]
                                  USING  'PNPOBJID'.

ENDFORM.                    " SELECT_OPTIONS
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_SELECT_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PNPPERNR[]  text
*      -->P_1134   text
*----------------------------------------------------------------------*
FORM preencher_select_option  TABLES   pt_option
                              USING    p_nmoption.

  FIELD-SYMBOLS: <fs_table> TYPE ANY TABLE,
                 <fs_estru> TYPE any.

  DATA: vl_text(40)     TYPE c.

* Criar field-symbol para estrutura
  ASSIGN (p_nmoption) TO <fs_estru>.

  CHECK sy-subrc EQ 0.

  CONCATENATE p_nmoption '[]'
  INTO vl_text.

* Criar field-symbol para tabela
  ASSIGN (vl_text) TO <fs_table>.

  CHECK sy-subrc EQ 0.

  CHECK NOT <fs_table> IS INITIAL.

  LOOP AT <fs_table> INTO <fs_estru>.
    CLEAR: wg_rsparams.
    MOVE-CORRESPONDING: <fs_estru> TO wg_rsparams.
    wg_rsparams-selname = p_nmoption.
    wg_rsparams-kind    = 'S'.
    APPEND wg_rsparams TO tg_rsparams.
  ENDLOOP.

ENDFORM.                    " PREENCHER_SELECT_OPTION
*&---------------------------------------------------------------------*
*&      Form  PAGAR_VARIANT
*&---------------------------------------------------------------------*
*   Apagar Variant
*----------------------------------------------------------------------*
FORM pagar_variant .

  CALL FUNCTION 'RS_VARIANT_DELETE'
    EXPORTING
      report               = sy-repid
      variant              = vg_varname
      flag_confirmscreen   = c_x
      flag_delallclient    = c_x
    EXCEPTIONS
      not_authorized       = 1
      not_executed         = 2
      no_report            = 3
      report_not_existent  = 4
      report_not_supplied  = 5
      variant_locked       = 6
      variant_not_existent = 7
      no_corr_insert       = 8
      variant_protected    = 9
      OTHERS               = 10.

ENDFORM.                    " PAGAR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  MUDAR_P_PRINT
*&---------------------------------------------------------------------*
*  Mudar botão P_PRINT
*----------------------------------------------------------------------*
FORM mudar_p_print .

  LOOP AT SCREEN.

    CHECK screen-name EQ 'P_PDEST'.

    IF p_print IS INITIAL.
      CLEAR: p_pdest.
      screen-input     = c_0.
    ELSE.
      screen-input     = c_1.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.                    " MUDAR_P_PRINT
*&---------------------------------------------------------------------*
*&      Form  SELECT_FUN_CARGO_DTADM
*&---------------------------------------------------------------------*
* Selecionar Funcionários por Cargo e Data de Admissão
*----------------------------------------------------------------------*
FORM select_fun_cargo_dtadm .

  DATA: tl_pa0041 TYPE TABLE OF pa0041,
        tl_pa0001 TYPE TABLE OF pa0001,
        wl_pa0001 TYPE          pa0001.

* Selecionar funcionário pela da de admissão
  REFRESH: tl_pa0041.
  SELECT *
  FROM pa0041
  INTO CORRESPONDING FIELDS OF TABLE tl_pa0041
  WHERE dat01 IN s_dtadm.

  IF sy-subrc EQ 0.
* Selecionar funcionário pelo cargo
    REFRESH: tl_pa0001.
    SELECT *
    FROM pa0001
    INTO CORRESPONDING FIELDS OF TABLE tl_pa0001
    FOR ALL ENTRIES IN tl_pa0041
    WHERE pernr EQ tl_pa0041-pernr
    AND stell IN s_cargo.
    IF sy-subrc EQ 0.

      SORT tl_pa0001 BY pernr.
      DELETE ADJACENT DUPLICATES FROM tl_pa0001 COMPARING pernr.

      LOOP AT tl_pa0001 INTO wl_pa0001.
* Gravar os funcionários selecionados no banco de dados lógico
        REFRESH: pnppernr.
        CLEAR: pnppernr.

        MOVE: wl_pa0001-pernr   TO pnppernr-low,
              'EQ'              TO pnppernr-option,
              'I'               TO pnppernr-sign.
        APPEND pnppernr.
      ENDLOOP.
    ELSE.
    ENDIF.
  ELSE.

  ENDIF.


ENDFORM.                    " SELECT_FUN_CARGO_DTADM
*&---------------------------------------------------------------------*
*&      Form  PRINT_ALL_TOGETHER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_all_together CHANGING vl_imprimiu TYPE char1.

*  DATA: DEST            TYPE TSP01-RQDEST   VALUE 'LOCL',
*        IMMEDIATE_PRINT TYPE TSP01-RQ1DISPO VALUE ' ',
*        HANDLE          TYPE SY-TABIX,
*        SPOOLID         TYPE TSP01-RQIDENT.

  DATA: buffer_otf   TYPE STANDARD TABLE OF soli,
        buffer_list  TYPE STANDARD TABLE OF tline,
        desired_type TYPE soodk-objtp VALUE 'OTF'.

  DATA: wa_tsp01 TYPE tsp01.

  DATA: pdf_tab      LIKE tline OCCURS 0 WITH HEADER LINE,
        bin_filesize TYPE i.

  DATA: lines      TYPE STANDARD TABLE OF itcoo,
        lines_otf  TYPE STANDARD TABLE OF itcoo,
        lines_list TYPE STANDARD TABLE OF tline.

  DATA: lv_filename    TYPE string,
        lv_path        TYPE string,
        lv_full_path   TYPE string,
        lv_extension   TYPE string,
        lv_user_action TYPE i.

  DATA: file_size TYPE i,
        filename  TYPE string.

  CHECK it_tsp01 IS NOT INITIAL.

  SORT it_tsp01 BY rqident.
  DELETE ADJACENT DUPLICATES FROM it_tsp01 COMPARING rqident.

*  "Abre uma nova ordem Spool com destino 'LOCL'
*  CALL FUNCTION 'RSPO_SR_OPEN'
*    EXPORTING
*      DEST             = DEST
*      IMMEDIATE_PRINT  = IMMEDIATE_PRINT
*    IMPORTING
*      HANDLE           = HANDLE
*      SPOOLID          = SPOOLID
*    EXCEPTIONS
*      DEVICE_MISSING   = 1
*      NAME_TWICE       = 2
*      NO_SUCH_DEVICE   = 3
*      OPERATION_FAILED = 4
*      OTHERS           = 5.
*
*  IF SY-SUBRC <> 0.
*    "Implement suitable error handling here
*  ENDIF.

  LOOP AT it_tsp01 INTO wa_tsp01.

*    IF WA_TSP01-RQDOCTYPE EQ 'OTF' OR WA_TSP01-RQDOCTYPE EQ 'SMART'.

    "Converter o conteúdo das ordens Spools tipo OTF e SMART salvas em IT_TSP01 p/ OTF.
    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
      EXPORTING
        rqident              = wa_tsp01-rqident
        desired_type         = desired_type
      TABLES
        buffer               = buffer_otf
      EXCEPTIONS
        no_such_job          = 1
        job_contains_no_data = 2
        selection_empty      = 3
        no_permission        = 4
        can_not_access       = 5
        read_error           = 6
        type_no_match        = 7
        OTHERS               = 8.

    IF sy-subrc <> 0.
      "DO NOTHING
    ELSE.
      APPEND LINES OF buffer_otf TO lines_otf.
    ENDIF.

*    ELSEIF WA_TSP01-RQDOCTYPE EQ 'LIST'.
*
*      "Converter o conteúdo das ordens Spools tipo LIST salvas em IT_TSP01 p/ OTF.
*      CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
*        EXPORTING
*          SRC_SPOOLID           = WA_TSP01-RQIDENT
*          NO_DIALOG             = 'X'
**         DST_DEVICE            =
**         PDF_DESTINATION       =
**      IMPORTING
**         PDF_BYTECOUNT         = numbytes
**         PDF_SPOOLID           = pdfspoolid
**         LIST_PAGECOUNT        =
**         BTC_JOBNAME           = jobname
**         BTC_JOBCOUNT          = jobcount
*        TABLES
*          PDF                   = BUFFER_LIST
*        EXCEPTIONS
*          ERR_NO_ABAP_SPOOLJOB  = 1
*          ERR_NO_SPOOLJOB       = 2
*          ERR_NO_PERMISSION     = 3
*          ERR_CONV_NOT_POSSIBLE = 4
*          ERR_BAD_DESTDEVICE    = 5
*          USER_CANCELLED        = 6
*          ERR_SPOOLERROR        = 7
*          OTHERS                = 8.
*
*      IF SY-SUBRC <> 0.
*        "DO NOTHING
*      ELSE.
*        APPEND LINES OF BUFFER_LIST TO LINES_LIST.
*      ENDIF.
*
*    ENDIF.

  ENDLOOP.

*  "Adicionar o conteúdo das ordens Spools convertidos em OTF na nova ordem Spool criada.
*  CALL FUNCTION 'RSPO_SR_TABLE_WRITE'
*    EXPORTING
*      HANDLE           = HANDLE
*    TABLES
*      LINES            = LINES
*    EXCEPTIONS
*      HANDLE_NOT_VALID = 1
*      OPERATION_FAILED = 2
*      OTHERS           = 3.
*
*  IF SY-SUBRC <> 0.
*    "Implement suitable error handling here
*  ENDIF.
*
*  "Fecha ordem Spool aberta
*  CALL FUNCTION 'RSPO_SR_CLOSE'
*    EXPORTING
*      HANDLE           = HANDLE
*    EXCEPTIONS
*      HANDLE_NOT_VALID = 1
*      OPERATION_FAILED = 2
*      OTHERS           = 3.
*
*  IF SY-SUBRC <> 0.
*    "Implement suitable error handling here
*  ENDIF.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
    IMPORTING
      bin_filesize          = bin_filesize
    TABLES
      otf                   = lines_otf
      lines                 = pdf_tab
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

  IF sy-subrc <> 0.
    "DO NOTHING
  ENDIF..

*  APPEND LINES OF LINES_LIST TO PDF_TAB.
  CHECK pdf_tab[] IS NOT INITIAL.

  lv_extension = 'pdf'.
  CLEAR: lv_filename, lv_path, lv_full_path.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = lv_extension
    CHANGING
      filename             = lv_filename
      path                 = lv_path
      fullpath             = lv_full_path
      user_action          = lv_user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc IS INITIAL AND lv_user_action NE 9.

    filename = lv_full_path.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize      = bin_filesize
        filename          = filename
        filetype          = 'BIN'
        confirm_overwrite = 'X'
      IMPORTING
        filelength        = file_size
      TABLES
        data_tab          = pdf_tab.

    vl_imprimiu = abap_true.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_DISPOSITIVO_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_dispositivo_saida .

  "Checa se o usuário possui um dispositivo de saída cadastrado na SU01 (Transação)

  DATA: wa_usr01 TYPE usr01.

  SELECT SINGLE *
  FROM usr01
  INTO wa_usr01
  WHERE bname = sy-uname.

  IF wa_usr01-spld IS INITIAL.
    MESSAGE i000(zhcm) WITH 'Usuário sem dispositivo de saída em SU01' DISPLAY LIKE c_e.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MARCAR_PF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM marcar_pf .
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_TD'.
        IF p_td IS NOT INITIAL.
          CLEAR: p_td.
          pf_01 = 'X'.  pf_02  = 'X'. "pf_03  = 'X'.  pf_04  = 'X'.
          "pf_05 = 'X'.  pf_06  = 'X'. pf_07  = 'X'.  pf_08  = 'X'.
          "pf_09 = 'X'.  pf_10  = 'X'. pf_11  = 'X'.  pf_12  = 'X'.
          "pf_13 = 'X'.  pf_14  = 'X'. pf_16  = 'X'.  pf_17  = 'X'.
*          pf_15 = 'X'.
          mc_todos = abap_false.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'PF_01'.
        IF pf_01 IS INITIAL.
          pf_01 = ''.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'PF_02'.
        IF pf_02 IS INITIAL.
          pf_02 = ''.
          MODIFY SCREEN.
        ENDIF.
*      WHEN 'PF_03'.
*        IF pf_03 IS INITIAL.
*          pf_03 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_04'.
*        IF pf_04 IS INITIAL.
*          pf_04 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_05'.
*        IF pf_05 IS INITIAL.
*          pf_05 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_06'.
*        IF pf_06 IS INITIAL.
*          pf_06 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_07'.
*        IF pf_07 IS INITIAL.
*          pf_07 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_08'.
*        IF pf_08 IS INITIAL.
*          pf_08 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_09'.
*        IF pf_09 IS INITIAL.
*          pf_09 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_10'.
*        IF pf_10 IS INITIAL.
*          pf_10 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_11'.
*        IF pf_11 IS INITIAL.
*          pf_11 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_12'.
*        IF pf_12 IS INITIAL.
*          pf_12 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_13'.
*        IF pf_13 IS INITIAL.
*          pf_13 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_14'.
*        IF pf_14 IS INITIAL.
*          pf_14 = ''.
*          MODIFY SCREEN.
*        ENDIF.
**      WHEN 'PF_15'.
**        IF pf_15 IS INITIAL.
**          pf_15 = ''.
**          MODIFY SCREEN.
**        ENDIF.
*      WHEN 'PF_16'.
*        IF pf_16 IS INITIAL.
*          pf_16 = ''.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PF_17'.
*        IF pf_17 IS INITIAL.
*          pf_17 = ''.
*          MODIFY SCREEN.
*        ENDIF.

    ENDCASE.
  ENDLOOP.
ENDFORM.
