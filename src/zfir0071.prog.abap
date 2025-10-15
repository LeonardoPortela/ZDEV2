*&---------------------------------------------------------------------*
*& Report  ZFIR0071
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir0071.

TABLES: zimp_aprovador, bkpf, zimp_cad_depto, t001, makt, zfit0043, zinv_aprovador, zsdt0064, zsdt0161.

*--- Criando a caixa de seleção única de tabelas a serem buscadas ---*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: zimp RADIOBUTTON GROUP a1 DEFAULT 'X' USER-COMMAND rg1,
              zgl  RADIOBUTTON GROUP a1,
              inv  RADIOBUTTON GROUP a1,
              admt RADIOBUTTON GROUP a1,
              lov  RADIOBUTTON GROUP a1,
              cred RADIOBUTTON GROUP a1,
              fret RADIOBUTTON GROUP a1,
              ovs  RADIOBUTTON GROUP a1,
              juri RADIOBUTTON GROUP a1.
SELECTION-SCREEN END OF BLOCK b2.

*--- Criando a tabela de seleção de Empresa, Departamento e Moeda ---*

*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS: S_BUKRS  FOR ZIMP_APROVADOR-BUKRS NO-EXTENSION NO INTERVALS,
*                S_MOEDA  FOR BKPF-WAERS,
*                S_DEPART FOR ZIMP_APROVADOR-DEP_RESP MATCHCODE OBJECT ZSH_DEP_RESP MODIF ID RG1,
*                S_OPER   FOR ZINV_APROVADOR-TP_OPERACAO  MODIF ID RG2,
*                S_MAT    FOR MAKT-MATNR MODIF ID RG2,
*                S_TRANSF FOR ZINV_APROVADOR-TRANSF_APROV,
*                S_WERKS  FOR ZSDT0064-WERKS MODIF ID RG3.
*PARAMETERS:     S_DATADE TYPE ZIMP_APROVADOR-DT_VAL_DE.
*PARAMETERS:     S_DATAAT TYPE ZIMP_APROVADOR-DT_VAL_ATE.
*SELECTION-SCREEN : COMMENT 52(62) TEXT-004 FOR FIELD S_DATAAT.
**                S_DATAAT TYPE ZIMP_APROVADOR-DT_VAL_ATE.
*SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs  FOR zimp_aprovador-bukrs NO-EXTENSION NO INTERVALS MODIF ID rg3,
                  s_moeda  FOR bkpf-waers MODIF ID rg3,
                  s_depart FOR zimp_aprovador-dep_resp MATCHCODE OBJECT zsh_dep_resp MODIF ID rg1,
                  s_oper   FOR zinv_aprovador-tp_operacao  MODIF ID rg2,
                  s_mat    FOR makt-matnr MODIF ID rg2,
                  s_transf FOR zinv_aprovador-transf_aprov MODIF ID rg3,
                  s_werks  FOR zsdt0064-werks MODIF ID rg4,
                  s_esc    FOR zsdt0161-vkbur MODIF ID rg6,
                  s_tvenda FOR zsdt0161-tp_venda MODIF ID rg5,
                  s_aprov  FOR zsdt0161-aprovador MODIF ID rg3.
  PARAMETERS: s_datade TYPE zimp_aprovador-dt_val_de MODIF ID rg3,
              s_dataat TYPE zimp_aprovador-dt_val_ate MODIF ID rg3.
  SELECTION-SCREEN : COMMENT 52(62) TEXT-004 FOR FIELD s_dataat MODIF ID rg3.
*                S_DATAAT TYPE ZIMP_APROVADOR-DT_VAL_ATE.
SELECTION-SCREEN END OF BLOCK b1.

*--- Criando ajuda para a campo de operação ---*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_oper-low.
  PERFORM f4_tp_oper.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_oper-high.
  PERFORM f4_tp_oper.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_esc-low.
  PERFORM f4_esc_venda.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_esc-high.
  PERFORM f4_esc_venda.



FORM f4_tp_oper.
  DATA: tl_return_tab_tp TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc_tp      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_tipoo OCCURS 0,
          tp_operacao TYPE zfit0043-tp_operacao,
          ds_operacao TYPE zfit0043-ds_operacao,
          "STATUS_CTB  TYPE ZFIT0043-STATUS_CTB,
          "LIQUIDAR    TYPE ZFIT0043-LIQUIDAR,
        END OF tl_tipoo.

  SELECT tp_operacao ds_operacao "STATUS_CTB LIQUIDAR
    FROM zfit0043
    INTO TABLE tl_tipoo
  WHERE spras = sy-langu.


  SORT tl_tipoo BY tp_operacao.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TP_OPERACAO'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0043-TP_OPERACAO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tipoo
      return_tab      = tl_return_tab_tp
      dynpfld_mapping = tl_dselc_tp.

*  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*    EXPORTING
*      FUNCTIONCODE           = '=ENT'
*    EXCEPTIONS
*      FUNCTION_NOT_SUPPORTED = 1
*      OTHERS                 = 2.
ENDFORM.                 " SEARCH_TIPO_OPERACAO  INPUT

*--- Criando a seleção dinâmica de campos para a pesquisa ---*

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'RG1'.
        IF ( inv EQ 'X' )  OR ( lov EQ 'X' ) OR ( cred EQ 'X' ) OR
           ( fret EQ 'X' ) OR ( ovs EQ 'X' ) OR ( juri EQ 'X').
          screen-active = 0.
          REFRESH: s_oper, s_mat.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'RG2'.
        IF ( zimp EQ 'X' ) OR ( zgl EQ 'X' )  OR ( admt EQ 'X' ) OR
           ( lov EQ 'X' )  OR ( cred EQ 'X' ) OR ( fret EQ 'X' ) OR
           ( ovs EQ 'X' )  OR ( juri EQ 'X').
          screen-active = 0.
          REFRESH: s_oper, s_mat.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'RG4'.
        IF ( lov EQ 'X' )  OR ( fret EQ 'X' ) OR ( ovs EQ 'X' ) OR ( juri EQ 'X').
          screen-active = 0.
          REFRESH: s_oper, s_mat.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'RG5'.
        IF ( zimp EQ 'X' ) OR ( zgl EQ 'X' )  OR ( admt EQ 'X' ) OR ( juri EQ 'X') OR
           ( inv EQ 'X' )  OR ( lov EQ 'X' )  OR ( cred EQ 'X' ) OR ( fret EQ 'X' ).
          screen-active = 0.
          REFRESH: s_oper, s_mat.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'RG6'.
        IF ( zimp EQ 'X' ) OR ( zgl EQ 'X' )  OR ( admt EQ 'X' ) OR
           ( cred EQ 'X' ) OR ( fret EQ 'X' ).
          screen-active = 0.
          REFRESH: s_oper, s_mat.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

END-OF-SELECTION.

*--- Criando a variável de títulos da tabela impressa ---*

  DATA titulo(99) TYPE c.

*--- Pegando a descrição do departamento e da empresa ---*

  DATA : it_zimp_cad_depto TYPE TABLE OF zimp_cad_depto,
         it_t001           TYPE TABLE OF t001,
         wa_zimp_cad_depto TYPE zimp_cad_depto,
         wa_t001           TYPE t001,
         depi_depto        TYPE string,
         depf_depto        TYPE string,
         emp_desc          TYPE string.

  IF s_bukrs IS NOT INITIAL.

    SELECT * FROM t001 INTO TABLE it_t001
    WHERE bukrs IN s_bukrs.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = s_bukrs-low.
    emp_desc = wa_t001-butxt.

  ENDIF.

  IF s_depart-low IS NOT INITIAL OR s_depart-high IS NOT INITIAL .

    SELECT * FROM zimp_cad_depto INTO TABLE it_zimp_cad_depto
    WHERE dep_resp IN s_depart.

    READ TABLE it_zimp_cad_depto INTO wa_zimp_cad_depto WITH KEY dep_resp = s_depart-low.
    depi_depto = wa_zimp_cad_depto-dep_resp_desc.

    READ TABLE it_zimp_cad_depto INTO wa_zimp_cad_depto WITH KEY dep_resp = s_depart-high.
    depf_depto = wa_zimp_cad_depto-dep_resp_desc.

  ENDIF.

*--- Criando Types para as possíveis tabelas de manipulação de dados e declarando-as ---*

  TYPES: BEGIN OF line_ty_zimp,
           dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc.
           INCLUDE STRUCTURE zimp_aprovador.
  TYPES: END OF line_ty_zimp.
  TYPES: table_ty_zimp TYPE STANDARD TABLE OF line_ty_zimp.

  TYPES: BEGIN OF line_ty_zgl,
           dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc.
           INCLUDE STRUCTURE zglt037.
  TYPES: END OF line_ty_zgl.
  TYPES: table_ty_zgl TYPE STANDARD TABLE OF line_ty_zgl.

  TYPES: BEGIN OF line_ty_inv,
           oper_desc     LIKE zfit0043-ds_operacao,
           mat_desc      LIKE makt-maktx,
           tipo_desc(50) TYPE c.
           INCLUDE STRUCTURE zinv_aprovador.
  TYPES: END OF line_ty_inv.
  TYPES: table_ty_inv TYPE STANDARD TABLE OF line_ty_inv.

  TYPES: BEGIN OF line_ty_adto,
           dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc.
           INCLUDE STRUCTURE zadto_aprovador.
  TYPES: END OF line_ty_adto.
  TYPES: table_ty_adto TYPE STANDARD TABLE OF line_ty_adto.

  DATA: it_saida_zimp      TYPE table_ty_zimp,
        wa_saida_zimp      TYPE line_ty_zimp,
        it_desc_zimp       TYPE TABLE OF zimp_cad_depto,
        wa_desc_zimp       TYPE zimp_cad_depto,
        it_saida_zgl       TYPE table_ty_zgl,
        wa_saida_zgl       TYPE line_ty_zgl,
        it_desc_zgl        TYPE TABLE OF zimp_cad_depto,
        wa_desc_zgl        TYPE zimp_cad_depto,
        it_saida_inv       TYPE table_ty_inv,
        wa_saida_inv       TYPE line_ty_inv,
        it_saida_adto      TYPE table_ty_adto,
        wa_saida_adto      TYPE line_ty_adto,
        it_saida_zsdt0064  TYPE STANDARD TABLE OF zsdt0064,
        wa_saida_zsdt0064  TYPE zsdt0064,
        it_saida_zsdt0161  TYPE TABLE OF zsdt0161,
        it_saida_zsdt0141  TYPE TABLE OF zsdt0141,
        it_saida_zsdt0152  TYPE TABLE OF zsdt0152,
        it_saida_zlest0156 TYPE TABLE OF zlest0156,
        it_saida_zsdt0385  TYPE TABLE OF zsdt0385,
        wa_saida_zsdt0385  TYPE zsdt0385,
        it_desc_adto       TYPE TABLE OF zimp_cad_depto,
        wa_desc_adto       TYPE zimp_cad_depto,
        it_desc_oper       TYPE TABLE OF zfit0043,
        wa_desc_oper       TYPE zfit0043,
        it_desc_mat        TYPE TABLE OF makt,
        wa_desc_mat        TYPE makt,
        lv_tabix           TYPE sy-tabix.

*--- Povoando as  tabelas / buscando as descrições dos departamentos / Definindo perfil de colunas da ALV / Chamando a ALV ---*

  IF zimp = 'X'.

    titulo = 'Relatório de Estratégias – ZIMP – Impostos'.

    IF s_bukrs-low IS INITIAL.
      SELECT * FROM zimp_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zimp
        WHERE bukrs IN s_bukrs
        AND dep_resp IN s_depart
        AND waers IN s_moeda
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ELSE.
      SELECT * FROM zimp_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zimp
      WHERE bukrs     LE s_bukrs-low
        AND bukrs_ate GE s_bukrs-low
        AND dep_resp  IN s_depart
        AND waers     IN s_moeda
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ENDIF.

    SELECT * FROM zimp_cad_depto INTO TABLE it_desc_zimp.

    lv_tabix = 0.

    LOOP AT it_saida_zimp INTO wa_saida_zimp.
      lv_tabix = sy-tabix.
      READ TABLE it_desc_zimp INTO wa_desc_zimp WITH KEY dep_resp = wa_saida_zimp-dep_resp.
      IF sy-subrc = 0.
        wa_saida_zimp-dep_resp_desc = wa_desc_zimp-dep_resp_desc.
        MODIFY it_saida_zimp FROM wa_saida_zimp INDEX lv_tabix TRANSPORTING dep_resp_desc.
      ENDIF.
    ENDLOOP.

    SORT it_saida_zimp BY bukrs ASCENDING
                          bukrs_ate ASCENDING
                          dep_resp ASCENDING
                          waers ASCENDING
                          nivel ASCENDING
                          aprovador ASCENDING
                          valor_de ASCENDING
                          valor_ate ASCENDING
                          data_atual ASCENDING
                          hora_atual ASCENDING
                          usuario ASCENDING.

    PERFORM alv1 USING:

   'BUKRS' 'Empr.De',
   'BUKRS_ATE' 'Empr.Até',
   'DEP_RESP' 'Depto.',
   'DEP_RESP_DESC' 'Nome Depto.',
   'WAERS' 'Moeda',
   'NIVEL' 'Nível',
   'APROVADOR' 'Aprovador',
   'VALOR_DE' 'Valor De',
   'VALOR_ATE' 'Valor Até',
   'DT_VAL_DE' 'Data De',
   'DT_VAL_ATE' 'Data Até',
   'MOTIVO' 'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL' 'Data Atualização',
   'HORA_ATUAL' 'Hora Atualização',
   'USUARIO' 'Usuário'.

    PERFORM alv-zimp.

  ELSEIF  zgl = 'X'.

    titulo = 'Relatório de Estratégias – ZGL – Lançamentos Manuais'.

    IF s_bukrs-low IS INITIAL.
      SELECT * FROM zglt037 INTO CORRESPONDING FIELDS OF TABLE it_saida_zgl
       WHERE bukrs IN s_bukrs
        AND dep_resp IN s_depart
        AND waers IN s_moeda
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ELSE.
      SELECT * FROM zglt037 INTO CORRESPONDING FIELDS OF TABLE it_saida_zgl
       WHERE bukrs     LE s_bukrs-low
        AND bukrs_ate GE s_bukrs-low
        AND dep_resp  IN s_depart
        AND waers     IN s_moeda
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ENDIF.

    SELECT * FROM zimp_cad_depto INTO TABLE it_desc_zgl.

    lv_tabix = 0.

    LOOP AT it_saida_zgl INTO wa_saida_zgl.
      lv_tabix = sy-tabix.
      READ TABLE it_desc_zgl INTO wa_desc_zgl WITH KEY dep_resp = wa_saida_zgl-dep_resp.
      IF sy-subrc = 0.
        wa_saida_zgl-dep_resp_desc = wa_desc_zgl-dep_resp_desc .
        MODIFY it_saida_zgl FROM wa_saida_zgl INDEX lv_tabix TRANSPORTING dep_resp_desc.
      ENDIF.
    ENDLOOP.

    SORT it_saida_zgl BY bukrs ASCENDING
                         bukrs_ate ASCENDING
                         dep_resp ASCENDING
                         dep_resp_desc ASCENDING
                         pgt_forn ASCENDING
                         waers ASCENDING
                         nivel ASCENDING
                         aprovador ASCENDING
                         valor_de ASCENDING
                         valor_ate ASCENDING
                         data_atual ASCENDING
                         hora_atual ASCENDING
                         usuario ASCENDING.

    PERFORM alv1 USING:

   'BUKRS' 'Empr.De',
   'BUKRS_ATE' 'Empr.Até',
   'DEP_RESP' 'Depto',
   'DEP_RESP_DESC'  'Nome Depto.',
   'PGT_FORN' 'Pag. Forn.',
   'WAERS' 'Moeda',
   'NIVEL' 'Nível',
   'APROVADOR' 'Aprovador',
   'VALOR_DE' 'Valor De',
   'VALOR_ATE' 'Valor Até',
   'DT_VAL_DE' 'Data De',
   'DT_VAL_ATE' 'Data Até',
   'MOTIVO' 'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL' 'Data Atualização',
   'HORA_ATUAL' 'Hora Atualização',
   'USUARIO' 'Usuário'.

    PERFORM alv-zgl.

  ELSEIF inv = 'X'.

    titulo = 'Relatório de Estratégias – Invoices'.

    IF s_bukrs-low IS INITIAL.
      SELECT * FROM zinv_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_inv
        WHERE bukrs IN s_bukrs
        AND waers IN s_moeda
        AND matnr IN s_mat
        AND tp_operacao IN s_oper
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ELSE.
      SELECT * FROM zinv_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_inv
        WHERE bukrs LE s_bukrs-low
        AND bukrs_ate GE s_bukrs-low
        AND waers IN s_moeda
        AND matnr IN s_mat
        AND tp_operacao IN s_oper
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ENDIF.

    SELECT * FROM zfit0043 INTO TABLE it_desc_oper.
    SELECT *
      FROM makt
      INTO TABLE it_desc_mat
    WHERE spras EQ sy-langu.

    lv_tabix = 0.

    LOOP AT it_saida_inv INTO wa_saida_inv.
      lv_tabix = sy-tabix.
      IF wa_saida_inv-tipo = '01'.
        wa_saida_inv-tipo_desc = 'INVOICE - Terceiros'.
      ELSEIF wa_saida_inv-tipo = '02'.
        wa_saida_inv-tipo_desc = 'Performance'.
      ELSEIF wa_saida_inv-tipo = '03'.
        wa_saida_inv-tipo_desc = 'Adiantamento'.
      ELSEIF wa_saida_inv-tipo = '04'.
        wa_saida_inv-tipo_desc = 'INVOICE - Grupo'.
      ENDIF.

      READ TABLE  it_desc_oper INTO wa_desc_oper WITH KEY tp_operacao = wa_saida_inv-tp_operacao.
      IF sy-subrc = 0.
        wa_saida_inv-oper_desc = wa_desc_oper-ds_operacao.
      ENDIF.

      READ TABLE  it_desc_mat INTO wa_desc_mat WITH KEY matnr = wa_saida_inv-matnr.
      IF sy-subrc = 0.
        wa_saida_inv-mat_desc = wa_desc_mat-maktx.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_saida_inv-matnr
        IMPORTING
          output = wa_saida_inv-matnr.

      MODIFY it_saida_inv FROM wa_saida_inv INDEX lv_tabix TRANSPORTING oper_desc tipo_desc mat_desc matnr.
    ENDLOOP.

    SORT it_saida_inv BY bukrs ASCENDING
                         bukrs_ate ASCENDING
                         tipo ASCENDING
                         waers ASCENDING
                         tp_operacao ASCENDING
                         matnr ASCENDING
                         nivel ASCENDING
                         aprovador ASCENDING
                         valor_de ASCENDING
                         valor_ate ASCENDING
                         data_atual ASCENDING
                         hora_atual ASCENDING
                         usuario ASCENDING.

    PERFORM alv1 USING:

   'BUKRS' 'Empr.De',
   'BUKRS_ATE' 'Empr.Até',
   'TIPO' 'Tipo',
   'TIPO_DESC' 'Descrição',
   'WAERS' 'Moeda',
   'TP_OPERACAO' 'Operação',
   'OPER_DESC' 'Descrição Operação',
   'MATNR' 'Material',
   'MAT_DESC' 'Descrição Material',
   'NIVEL' 'Nivel',
   'APROVADOR' 'Aprovador',
   'VALOR_DE' 'Valor De',
   'VALOR_ATE' 'Valor Até',
   'DT_VAL_DE' 'Data De',
   'DT_VAL_ATE' 'Data Até',
   'MOTIVO' 'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL' 'Data Atualização',
   'HORA_ATUAL' 'Hora Atualização',
   'USUARIO' 'Usuário'.

    PERFORM alv-aprov.

  ELSEIF admt = 'X'.

    titulo = 'Relatório de Estratégias – Adiantamentos'.

    IF s_bukrs-low IS INITIAL.
      SELECT * FROM zadto_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_adto
        WHERE bukrs    IN s_bukrs
          AND dep_resp IN s_depart
          AND waers    IN s_moeda
          AND transf_aprov IN s_transf
          AND aprovador IN s_aprov
          AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ELSE.
      SELECT * FROM zadto_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_adto
      WHERE bukrs     LE s_bukrs-low
        AND bukrs_ate GE s_bukrs-low
        AND dep_resp  IN s_depart
        AND waers     IN s_moeda
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
      AND dt_val_ate GE s_dataat.
    ENDIF.

    SELECT * FROM zimp_cad_depto INTO TABLE it_desc_adto.

    lv_tabix = 0.

    LOOP AT it_saida_adto INTO wa_saida_adto.
      lv_tabix = sy-tabix.
      READ TABLE it_desc_adto INTO wa_desc_adto WITH KEY dep_resp = wa_saida_adto-dep_resp.
      IF sy-subrc = 0.
        wa_saida_adto-dep_resp_desc = wa_desc_adto-dep_resp_desc.
        MODIFY it_saida_adto FROM wa_saida_adto INDEX lv_tabix TRANSPORTING dep_resp_desc.
      ENDIF.
    ENDLOOP.

    SORT it_saida_adto BY bukrs ASCENDING
                          bukrs_ate ASCENDING
                          dep_resp ASCENDING
                          waers ASCENDING
                          nivel ASCENDING
                          aprovador ASCENDING
                          valor_de ASCENDING
                          valor_ate ASCENDING
                          data_atual ASCENDING
                          hora_atual ASCENDING
                          usuario ASCENDING.

    PERFORM alv1 USING:

   'BUKRS' 'Empr.De',
   'BUKRS_ATE' 'Empr.Até',
   'DEP_RESP' 'Depto',
   'DEP_RESP_DESC' 'Nome Depto.',
   'WAERS' 'Moeda',
   'NIVEL' 'Nível',
   'APROVADOR' 'Aprovador',
   'VALOR_DE' 'Valor De',
   'VALOR_ATE' 'Valor Até',
   'DT_VAL_DE' 'Data De',
   'DT_VAL_ATE' 'Data Até',
   'MOTIVO' 'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL' 'Data Atualização',
   'HORA_ATUAL' 'Hora Atualização',
   'USUARIO' 'Usuário'.

    PERFORM alv-adto.

  ELSEIF ovs EQ abap_true.

    titulo = 'Relatório de Estratégias – Ordens de Venda'.


    SELECT * FROM zsdt0161 INTO CORRESPONDING FIELDS OF TABLE it_saida_zsdt0161
      WHERE bukrs    IN s_bukrs
        AND vkbur    IN s_esc
        AND tp_venda IN s_tvenda
        AND waers    IN s_moeda
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
    AND dt_val_ate GE s_dataat.

    SORT it_saida_zsdt0161 BY bukrs vkbur tp_venda nivel ASCENDING.

    PERFORM alv1 USING:

   'BUKRS'        'Empr.De',
   'BUKRS_ATE'    'Empr.Até',
   'VKBUR'        'Escritório de vendas',
   'VKBUR_ATE'    'Escritório de vendas Até',
   'TP_VENDA'	    'Tipo de venda',
   'TP_VENDA_ATE'	'Tipo de venda Até',
   'WAERS'        'Moeda',
   'NIVEL'        'Nivel Aprovação',
   'APROVADOR'    'Aprovador',
   'VALOR_DE'     'Valor De',
   'VALOR_ATE'    'Valor Até',
   'DT_VAL_DE'    'Data De',
   'DT_VAL_ATE'   'Data Até',
   'MOTIVO'       'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL'   'Data Atualização',
   'HORA_ATUAL'   'Hora Atualização',
   'USUARIO'      'Usuário'.

    PERFORM alv-ovs.

  ELSEIF fret EQ abap_true.

    titulo = 'Relatório de Estratégias – Preço Frete'.


    SELECT * FROM zlest0156 INTO CORRESPONDING FIELDS OF TABLE it_saida_zlest0156
      WHERE bukrs    IN s_bukrs
        AND transf_aprov IN s_transf
        AND aprovador IN s_aprov
        AND dt_val_de GE s_datade
    AND dt_val_ate GE s_dataat.

    SORT it_saida_zlest0156 BY bukrs nivel ASCENDING.

    PERFORM alv1 USING:

   'BUKRS'        'Empr.De',
   'BUKRS_ATE'    'Empr.Até',
   'NIVEL'        'Nivel Aprovação',
   'APROVADOR'    'Aprovador',
   'VALOR_DE'     'Valor De',
   'VALOR_ATE'    'Valor Até',
   'DT_VAL_DE'    'Data De',
   'DT_VAL_ATE'   'Data Até',
   'MOTIVO'       'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL'   'Data Atualização',
   'HORA_ATUAL'   'Hora Atualização',
   'USUARIO'      'Usuário'.

    PERFORM alv-fret.

  ELSEIF lov EQ abap_true.

    titulo = 'Relatório de Estratégias – Lançamento OV'.


    SELECT * FROM zsdt0141 INTO CORRESPONDING FIELDS OF TABLE it_saida_zsdt0141
      WHERE bukrs         IN s_bukrs
        AND vkbur         IN s_esc
        AND waers         IN s_moeda
        AND transf_aprov  IN s_transf
        AND aprovador     IN s_aprov
        AND dt_val_de     GE s_datade
    AND dt_val_ate    GE s_dataat.

    SORT it_saida_zsdt0141 BY bukrs vkbur nivel ASCENDING.

    PERFORM alv1 USING:

   'BUKRS'        'Empr.De',
   'BUKRS_ATE'    'Empr.Até',
   'VKBUR'        'Escritório de vendas',
   'VKBUR_ATE'    'Escritório de vendas Até',
   'WAERS'        'Moeda',
   'NIVEL'        'Nivel Aprovação',
   'APROVADOR'    'Aprovador',
   'VALOR_DE'     'Valor De',
   'VALOR_ATE'    'Valor Até',
   'DT_VAL_DE'    'Data De',
   'DT_VAL_ATE'   'Data Até',
   'MOTIVO'       'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL'   'Data Atualização',
   'HORA_ATUAL'   'Hora Atualização',
   'USUARIO'      'Usuário'.

    PERFORM alv-lov.

  ELSEIF cred EQ abap_true.

    titulo = 'Relatório de Estratégias – Limite de Crédito'.


    SELECT * FROM zsdt0152 INTO CORRESPONDING FIELDS OF TABLE it_saida_zsdt0152
      WHERE vkorg         IN s_bukrs
        AND werks         IN s_werks
        AND transf_aprov  IN s_transf
        AND aprovador     IN s_aprov
        AND dt_val_de     GE s_datade
    AND dt_val_ate    GE s_dataat.

    SORT it_saida_zsdt0152 BY vkorg werks nivel ASCENDING.

    PERFORM alv1 USING:

   'VKORG'        'Organização de vendas',
   'WERKS'        'Centro',
   'WERKS_ATE'    'Centro Até',
   'NIVEL'        'Nivel Aprovação',
   'APROVADOR'    'Aprovador',
   'VALOR_DE'     'Valor De',
   'VALOR_ATE'    'Valor Até',
   'DT_VAL_DE'    'Data De',
   'DT_VAL_ATE'   'Data Até',
   'MOTIVO'       'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL'   'Data Atualização',
   'HORA_ATUAL'   'Hora Atualização',
   'USUARIO'      'Usuário'.

    PERFORM alv-cred.


  ELSEIF juri EQ 'X'.
    titulo = 'Relatório de Estratégias – Questionamento Jurídico'.


    SELECT * FROM zsdt0385 INTO CORRESPONDING FIELDS OF TABLE it_saida_zsdt0385
      WHERE bukrs         IN s_bukrs
        AND vkbur         IN s_esc
*          AND WAERS         IN S_MOEDA
        AND transf_aprov  IN s_transf
        AND aprovador     IN s_aprov
        AND dt_val_de     GE s_datade
    AND dt_val_ate    GE s_dataat.

    SORT it_saida_zsdt0385 BY bukrs vkbur nivel ASCENDING.

    PERFORM alv1 USING:

   'BUKRS'        'Empr.De',
   'BUKRS_ATE'    'Empr.Até',
   'VKBUR'        'Escritório de vendas',
   'VKBUR_ATE'    'Escritório de vendas Até',
*   'WAERS'        'Moeda',
   'NIVEL'        'Nivel Aprovação',
   'APROVADOR'    'Aprovador',
   'VALOR_DE'     'Valor De',
   'VALOR_ATE'    'Valor Até',
   'DT_VAL_DE'    'Data De',
   'DT_VAL_ATE'   'Data Até',
   'MOTIVO'       'Motivo',
   'TRANSF_APROV' 'Transferência',
   'DATA_ATUAL'   'Data Atualização',
   'HORA_ATUAL'   'Hora Atualização',
   'USUARIO'      'Usuário'.

    PERFORM alv-juri.

  ENDIF.

*--- Inicializando o fieldcat ---*

  TYPE-POOLS: slis.

  DATA: wa_fcat TYPE slis_fieldcat_alv,
        it_fcat TYPE slis_t_fieldcat_alv.

*--- Form que cria os nomes das colunas no grid da fieldcat ---*

FORM alv1  USING   p_campo p_desc.

  wa_fcat-fieldname = p_campo.
  wa_fcat-seltext_m = p_desc.

  APPEND wa_fcat TO it_fcat.

ENDFORM.

*--- Form para impressão da ALV ---*

DATA: wa_fieldlayout  TYPE    slis_layout_alv.

FORM alv-zimp.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_zimp
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-zgl.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_zgl
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-aprov.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_inv
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-adto.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_adto
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-ovs.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_zsdt0161
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-fret.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_zlest0156
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-lov.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_zsdt0141
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-cred.

  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_zsdt0385
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv-juri.
  wa_fieldlayout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      it_fieldcat            = it_fcat
      is_layout              = wa_fieldlayout
    TABLES
      t_outtab               = it_saida_zsdt0385
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
*--- Formulário contendo o cabeçalho da tabela impressa ---"

FORM top_of_page.
* Declarações locais do cabeçalho do ALV

  DATA: it_header     TYPE slis_t_listheader,
        st_header     TYPE slis_listheader,
        t_line        LIKE st_header-info,
        ld_lines      TYPE i,
        ld_linesc(10) TYPE c.

  "  Entrando no tipo da estrutura do header (slis_listheader), você verá
  " que esta estrutura tem 3 tipos e eu explicarei cada um deles abaixo.

  "  Titulo ( H )
  " Vai ser a linha que mais chama a atenção dentro do Cabeçalho, pois será
  " maior e em negrito.

  st_header-typ  = 'H'.
  st_header-info = titulo.
  APPEND st_header TO it_header.
  CLEAR st_header.

  " Informações ( S )
  "  Como se fosse o texto para comentários, descrição de algo, somente no
  " tipo 'S' o campo 'KEY' faz algo, ele serve como inicio da sua frase e fica
  " em negrito se diferenciando do resto da linha.

  st_header-typ  = 'S'.
  st_header-key = 'Empresa: '.

  IF s_bukrs IS NOT INITIAL.
    CONCATENATE s_bukrs-low emp_desc INTO st_header-info SEPARATED BY ' - '.
    APPEND st_header TO it_header.
  ENDIF.

*  IF OVS EQ ABAP_FALSE.
*    APPEND ST_HEADER TO IT_HEADER.
*  ENDIF.

  CLEAR: st_header.

  IF inv <> 'X'.
    st_header-typ  = 'S'.
    st_header-key = 'Departamento: '.
    IF s_depart IS NOT INITIAL.
      IF s_depart-high = ''.
        st_header-info = depi_depto.
        CONCATENATE s_depart-low depi_depto INTO st_header-info SEPARATED BY ' - '.
      ELSEIF s_depart-low = '' AND s_depart-high <> ''.
        CONCATENATE: s_depart-high ' - ' depf_depto INTO st_header-info SEPARATED BY space.
      ELSE.
        CONCATENATE: s_depart-low ' - ' depi_depto 'até' s_depart-high ' - ' depf_depto INTO st_header-info SEPARATED BY space.
      ENDIF.
      APPEND st_header TO it_header.
    ENDIF.
  ENDIF.

*  IF OVS EQ ABAP_FALSE.
*    APPEND ST_HEADER TO IT_HEADER.
*  ENDIF.

  CLEAR: st_header.

  st_header-typ  = 'S'.
  st_header-key = 'Moeda: '.
  IF s_moeda IS NOT INITIAL.
    IF s_moeda-high = ''.
      st_header-info = s_moeda-low.
    ELSE.
      CONCATENATE: s_moeda-low 'até' s_moeda-high INTO st_header-info SEPARATED BY space.
    ENDIF.

    APPEND st_header TO it_header.

  ENDIF.

*  IF OVS EQ ABAP_FALSE.
*    APPEND ST_HEADER TO IT_HEADER.
*  ENDIF.

  CLEAR: st_header.

  st_header-typ  = 'S'.
  st_header-key = 'Centro: '.
  IF s_werks IS NOT INITIAL.
    IF s_werks-high = ''.
      st_header-info = s_werks-low.
    ELSE.
      CONCATENATE: s_werks-low 'até' s_werks-high INTO st_header-info SEPARATED BY space.
    ENDIF.
    APPEND st_header TO it_header.
  ENDIF.

*  IF OVS EQ ABAP_TRUE.
*    APPEND ST_HEADER TO IT_HEADER.
*  ENDIF.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     i_alv_form         = 'X'
    .

  "  Ai está a função que vai junta tudo isso e mostra no inicio da página,
  " podemos até adicionar uma imagem que esteja no banco SAP ( OAER ), Mas
  " isso é assunto para um próximo Post,
  " Qualquer dúvida estamos aih.


*-------------------------------------------------------------------

*DATA : IT_FCAT            TYPE SLIS_T_FIELDCAT_ALV,
*        WA_FCAT            TYPE SLIS_FIELDCAT_ALV,
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_ESC_VENDA
*&---------------------------------------------------------------------*
FORM f4_esc_venda.

  DATA: tl_return_tab_tp TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc_tp      TYPE TABLE OF dselc      WITH HEADER LINE.

  SELECT vkbur, bezei FROM tvkbt INTO TABLE @DATA(tl_tvkbt) WHERE spras EQ 'P' ORDER BY vkbur.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VKBUR'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'S_ESC-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tvkbt
      return_tab      = tl_return_tab_tp
      dynpfld_mapping = tl_dselc_tp.

ENDFORM.
