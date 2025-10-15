*&---------------------------------------------------------------------*
*& Report  ZGLT068
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zglt068 MESSAGE-ID zfi.

TABLES: zglt_dre_04, sscrfields.

DATA: ls_variant  TYPE disvariant,
      l_exit      TYPE char1,
      def_variant TYPE disvariant,
      variant     TYPE disvariant,
      variante    LIKE disvariant.

DATA: vg_repid   LIKE sy-repid,
      gs_variant TYPE disvariant,
      vg_variant TYPE disvariant.

TYPES:BEGIN OF y_saida,
        bukrs        TYPE zglt_dre_04-bukrs,
        gjahr        TYPE zglt_dre_04-gjahr,
        docnr        TYPE zglt_dre_04-docnr,
        docln        TYPE zglt_dre_04-docln,
        belnr        TYPE zglt_dre_04-belnr,
        buzei        TYPE zglt_dre_04-buzei,
        poper        TYPE zglt_dre_04-poper,
        budat        TYPE zglt_dre_04-budat,
        awtyp        TYPE zglt_dre_04-awtyp,
        saknr        TYPE zglt_dre_04-saknr,
        ktopl        TYPE zglt_dre_04-ktopl,
        kokrs        TYPE zglt_dre_04-kokrs,
        shkzg        TYPE zglt_dre_04-shkzg,
        kostl        TYPE zglt_dre_04-kostl,
        kosar        TYPE zglt_dre_04-kosar,
        kosar_d      TYPE zglt_alv-kosar_d,
        prctr        TYPE zglt_dre_04-prctr,
        matnr        TYPE zglt_dre_04-matnr,
        matkl        TYPE zglt_dre_04-matnr,
        aufnr        TYPE zglt_dre_04-aufnr,
        tp_ordem     TYPE zztp_ordem,
        vbund        TYPE zglt_dre_04-vbund,
        rtcur        TYPE zglt_dre_04-rtcur,
        vltsl        TYPE zglt_dre_04-vltsl,
        vlhsl        TYPE zglt_dre_04-vlhsl,
        vlksl        TYPE zglt_dre_04-vlksl,
        vlosl        TYPE zglt_dre_04-vlosl,
        runit        TYPE zglt_dre_04-runit,
        qtmsl        TYPE zglt_dre_04-qtmsl,
        tp_conta     TYPE zglt_dre_04-tp_conta,
        nivel        TYPE zglt_dre_04-nivel,
        timestamp    TYPE zglt_dre_04-timestamp,
        ck_integrado TYPE zglt_dre_04-ck_integrado,
        observacao   TYPE zglt_dre_04-observacao,
        runit_base   TYPE zglt_dre_04-runit_base,
        qtmsl_base   TYPE zglt_dre_04-qtmsl_base,
        ds_usuario   TYPE zglt_dre_04-ds_usuario,
        dt_alteracao TYPE zglt_dre_04-dt_alteracao,
        hr_alteracao TYPE zglt_dre_04-hr_alteracao,
        zcheck       TYPE zglt_dre_04-zcheck,
        tp_conta_des TYPE zztpconta,
      END OF y_saida.


TYPES: BEGIN OF ty_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF ty_ucomm.

DATA: icon_proc TYPE string.

*(Obrigatório)
*BUKRS  BUKRS CHAR  4 0 Empresa
*(Obrigatório)
*GJAHR  GJAHR NUMC  4 0 Exercício
*(Obrigatório)
*POPER  POPER NUMC  3 0 Período contábil
*DOCNR  BELNR_D CHAR  10  0 Nº documento de um documento contábil
*DOCLN  DOCLN6  CHAR  6 0 Linha de lançamento de 6 caracteres p/ledger
*BELNR  BELNR_D CHAR  10  0 Nº documento de um documento contábil
*BUZEI  BUZEI NUMC  3 0 Nº linha de lançamento no documento contábil
*BUDAT  BUDAT DATS  8 0 Data de lançamento no documento
*SAKNR  SAKNR CHAR  10  0 Nº conta do Razão
*KTOPL  KTOPL CHAR  4 0 Plano de contas
*KOKRS  KOKRS CHAR  4 0 Área de contabilidade de custos
*SHKZG  SHKZG CHAR  1 0 Código débito/crédito
*KOSTL  KOSTL CHAR  10  0 Centro de custo
*KOSAR  KOSAR CHAR  1 0 Tipo de centro de custo
*PRCTR  PRCTR CHAR  10  0 Centro de lucro
*MATNR  MATNR CHAR  18  0 Nº do material
*MATKL  MATKL CHAR  9 0 Grupo de mercadorias
*AUFNR  AUFNR CHAR  12  0 Nº ordem
*VBUND  VBUND CHAR  6 0 Nº sociedade

DATA: it_zglt_dre_04 TYPE TABLE OF y_saida WITH HEADER LINE,
      wa_saida       TYPE  y_saida,
      it_saida       TYPE TABLE OF zglt_dre_04 WITH HEADER LINE,
*      it_zglt_dre_04 TYPE TABLE OF zglt_dre_04 WITH HEADER LINE,
      ok_code        TYPE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK filtros WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR zglt_dre_04-bukrs OBLIGATORY ,
                  p_gjahr FOR zglt_dre_04-gjahr OBLIGATORY NO INTERVALS NO-EXTENSION,
                  p_poper FOR zglt_dre_04-poper OBLIGATORY ,
                  p_docnr FOR zglt_dre_04-docnr,
                  p_docln FOR zglt_dre_04-docln,
                  p_belnr FOR zglt_dre_04-belnr,
                  p_buzei FOR zglt_dre_04-buzei,
                  p_budat FOR zglt_dre_04-budat,
                  p_saknr FOR zglt_dre_04-saknr,
                  p_ktopl FOR zglt_dre_04-ktopl,
                  p_kokrs FOR zglt_dre_04-kokrs,
                  p_shkzg FOR zglt_dre_04-shkzg,
                  p_kostl FOR zglt_dre_04-kostl,
                  p_kosar FOR zglt_dre_04-kosar,
                  p_prctr FOR zglt_dre_04-prctr,
                  p_matnr FOR zglt_dre_04-matnr,
                  p_matkl FOR zglt_dre_04-matkl,
                  p_aufnr FOR zglt_dre_04-aufnr,
                  p_vbund FOR zglt_dre_04-vbund.
  PARAMETERS: vobjeto TYPE char01 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK filtros.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-012.
  PARAMETERS: p_var TYPE disvariant-variant.
  PARAMETERS:p_al11 AS CHECKBOX.
  PARAMETERS:p_aqr TYPE bseg-sgtxt.

SELECTION-SCREEN: END OF BLOCK b3.

"Declaração botão 01
SELECTION-SCREEN FUNCTION KEY 1. "Função para criar o botãona tela para acessar ZGL086



*alterado por Guilherme rabelo inicio 15.05.2023
INITIALIZATION.
*  SET PF-STATUS 'PF0101'. "Comentado.

  "Botão para abrir ZCL086
  sscrfields-functxt_01 = 'Tipo centro De Custo'.


**********************************************************************
* Function List Variant Report - 115303 CS2023000352 Inclusão do "Salvar Variantes"
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM: f4_variant.



*&---------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*

  "=======================================================================
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*&---------------------------------------------------------------------*
  " Botões da tela inicial
  CASE sy-ucomm.
*    WHEN '&TIPO_C&'.
*       Chama SM30

*      CALL TRANSACTION 'ZGL086'.
*      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
*        EXPORTING
*          action    = 'U'
*          view_name = 'ZBRTAX007_T'.

*    WHEN 'EXEC' OR '&EXEC'.

*      PERFORM: limpar_tabelas,
*          selecionar_registros.

      "IF IT_ZGLT_DRE_04[] IS INITIAL.
      "  MESSAGE S058.
      "ELSE.
*      PERFORM: mostrar_dados.
      "ENDIF.

*    WHEN OTHERS.
*      LEAVE PROGRAM.
    WHEN 'FC01'.
      CALL TRANSACTION 'ZGL086'.
  ENDCASE.
*alterado por Guilherme rabelo inicio 15.05.2023
  "===================================================

START-OF-SELECTION.

  PERFORM: limpar_tabelas,
          selecionar_registros.

  IF it_zglt_dre_04[] IS INITIAL.
    MESSAGE s058.
  ELSE.
    PERFORM: mostrar_dados.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_tabelas .
  CLEAR: it_zglt_dre_04[].
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_registros.

  DATA:values_tab	  TYPE TABLE OF	dd07v,
       values_dd07l	TYPE TABLE OF	dd07l.

  IF sy-tcode = 'ZDRE0003'.
    LOOP AT p_poper.
      IF p_poper-low = 12.
        p_poper-low    = 13.
        p_poper-sign   = 'I'.
        p_poper-option = 'EQ'.
        APPEND p_poper.
        "
        p_poper-low    = 14.
        p_poper-sign   = 'I'.
        p_poper-option = 'EQ'.
        APPEND p_poper.
        "
        p_poper-low    = 15.
        p_poper-sign   = 'I'.
        p_poper-option = 'EQ'.
        APPEND p_poper.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDIF.

  SELECT * INTO TABLE it_saida"it_zglt_dre_04
    FROM zglt_dre_04
   WHERE bukrs IN p_bukrs
     AND gjahr IN p_gjahr
     AND poper IN p_poper
     AND docnr IN p_docnr
     AND docln IN p_docln
     AND belnr IN p_belnr
     AND buzei IN p_buzei
     AND budat IN p_budat
     AND saknr IN p_saknr
     AND ktopl IN p_ktopl
     AND kokrs IN p_kokrs
     AND shkzg IN p_shkzg
     AND kostl IN p_kostl
     AND kosar IN p_kosar
     AND prctr IN p_prctr
     AND matnr IN p_matnr
     AND matkl IN p_matkl
     AND aufnr IN p_aufnr
     AND vbund IN p_vbund.

*alterado por Guilherme rabelo inicio
  IF sy-subrc = 0.

    SELECT * FROM zgl015_dre_ccust
      INTO TABLE @DATA(it_zgl015_dre_ccusto)
      FOR ALL ENTRIES IN @it_saida
      WHERE kosar = @it_saida-kosar.

    SELECT * FROM coas
      INTO TABLE @DATA(it_coas)
      FOR ALL ENTRIES IN @it_saida
       WHERE  aufnr = @it_saida-aufnr.


    LOOP AT it_saida INTO DATA(wa_zglt_dre_04).

      MOVE-CORRESPONDING wa_zglt_dre_04 TO wa_saida.

      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname      = 'ZZTPCONTA'
*         TEXT         = 'X'
*         FILL_DD07L_TAB        = ' '
        TABLES
          values_tab   = values_tab
          values_dd07l = values_dd07l.


      READ TABLE values_tab INTO DATA(ls_value) WITH KEY domvalue_l = wa_saida-tp_conta.

      IF sy-subrc = 0.

        CONCATENATE ls_value-domvalue_l '-' ls_value-ddtext INTO wa_saida-tp_conta_des.

      ENDIF.

      IF wa_saida-kosar <> ' '.

        READ TABLE it_zgl015_dre_ccusto INTO DATA(wa_zgl015_dre_ccusto) WITH KEY kosar = wa_saida-kosar.

        IF sy-subrc = 0.

          wa_saida-kosar_d = wa_zgl015_dre_ccusto-tp_ccusto.


        ENDIF.
      ENDIF.

      IF wa_saida-aufnr <> ' '.

        READ TABLE it_coas INTO DATA(wa_coas) WITH KEY aufnr  = wa_saida-aufnr .

        IF sy-subrc = 0.
          wa_saida-tp_ordem  = wa_coas-auart.
        ENDIF.

      ENDIF.

      APPEND wa_saida  TO it_zglt_dre_04.
      CLEAR:wa_saida.
    ENDLOOP.



  ENDIF.


*  verifica se vai ter arquivo na al11

  DATA: it_txt     TYPE truxs_t_text_data,
        it_txt_cab TYPE string,
        str1       TYPE string,
        str        TYPE string.

  IF p_al11 = 'X'.

    CONCATENATE 'Empresa'
                'Ano'
                'Nº documento'
                'Linhlançto'
                'Nº documento'
                'Item'
                'Período'
                'Data de lançamento'
                'Op.ref'
                'Conta Razão'
                'PlCt'
                'ÁCC'
                'D/C'
                'Centro de Custo'
                'Tipo Centro de custo'
                'Desc Centro de custo'
                'Centro de Lucro'
                'Material'
                'Grp Mercadoria'
                'Ordem'
                'Tp Ordem'
                'N sociedade'
                'Moeda'
                'Montante MT'
                'Montante MI'
                'Montante MGE'
                'Montante OM'
                'UMB'
                'Quantidade'
                'Tipo Conta'
                'Nivel DRE'
                'Registro da Hora'
                'Integrado'
                'Observação'
                'UMB'
                'Quantidade'
                'Usuario'
                'Data Atual'
                'Hora'
                ' '
                'Desc Tipo centro'
                 INTO it_txt_cab SEPARATED BY '|'.


    CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
      EXPORTING
        i_field_seperator    = '|'
      TABLES
        i_tab_sap_data       = it_zglt_dre_04
      CHANGING
        i_tab_converted_data = it_txt.


    CONCATENATE '/usr/sap/SPED/' p_aqr '_' sy-datum sy-uzeit '.txt'
      INTO str.


    OPEN DATASET str FOR APPENDING IN TEXT MODE ENCODING DEFAULT WITH SMART LINEFEED .

    IF sy-subrc = 0.

      TRANSFER it_txt_cab TO str.

      LOOP AT it_txt INTO DATA(wa_es_tabt).

        TRANSFER wa_es_tabt TO str.

      ENDLOOP.
    ENDIF.

    CLOSE DATASET str.

  ENDIF.

*alterado por Guilherme rabelo fim
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_dados .

  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECT_VARIAVEL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_select_variavel_alv .



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  f4_variant .
  CLEAR: gs_variant.
  gs_variant-report = sy-repid.

  IF ( NOT p_var IS INITIAL ).
    gs_variant-variant = p_var.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = gs_variant
      i_save     = 'A'
    IMPORTING
*     e_exit     = l_exit
      es_variant = gs_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_var = gs_variant-variant.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VARIANT_V
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_variant_v .
  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_var IS INITIAL ).
    vg_variant-variant = p_var.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      e_exit        = l_exit
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    p_var =  variante-variant.
  ELSE.
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ENDIF.
ENDFORM.


INCLUDE zglt068_0100.

INCLUDE zglt068_0002.
*&---------------------------------------------------------------------*
*&      Form  LF_VAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lf_var .

ENDFORM.
