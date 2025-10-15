*&---------------------------------------------------------------------*
*& Report  ZSDR0132_APROVAR                                            *
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Realização de Baixa de Volume de NF de compra           *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome     | Request | Descrição                             *
*----------------------------------------------------------------------*
* 20.04.21| JBARBOSA | Request | Desenvolvimento Inicial               *
*----------------------------------------------------------------------*
REPORT zsdr0132_aprovar.

*----------------------------------------------------------------------*
* Declaração de Constantes
*----------------------------------------------------------------------*
CONSTANTS: BEGIN OF c_const,
             tabela_cad    TYPE tabname VALUE 'ZSDT0278',
             layout_cad    TYPE tabname VALUE 'ZSDT0278_OUT',
             tela_cad      TYPE c LENGTH 4 VALUE '0098',
             titulo_cad    TYPE cua_tit_tx VALUE 'Cadastro Aprovador -  Baixa de Notas Fiscais de Compra',
             set_user      TYPE setleaf-setname VALUE 'Z_ZSDT0182_USER',

             matkl_algodao TYPE mara-matkl VALUE '000700140',
             meins_kg      TYPE mara-meins VALUE 'KG',

           END OF c_const.

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: j_1bnfdoc,
        j_1bnflin,
        zsdt0276.

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         id_baixa      TYPE zsdt0276-id_baixa,
         bukrs         TYPE j_1bnfdoc-bukrs,
         branch        TYPE j_1bnfdoc-branch,
         docnum        TYPE j_1bnfdoc-docnum,
         itmnum        TYPE j_1bnflin-itmnum,
         nfe           TYPE j_1bnfdoc-nfe,
         nfenum        TYPE j_1bnfdoc-nfenum,
         credat        TYPE j_1bnfdoc-credat,
         docdat        TYPE j_1bnfdoc-docdat,
         partyp        TYPE j_1bnfdoc-partyp,
         parid         TYPE j_1bnfdoc-parid,
         name1         TYPE lfa1-name1,
         regio         TYPE lfa1-regio,
         matnr         TYPE mara-matnr,
         maktx         TYPE makt-maktx,
         meins         TYPE j_1bnflin-meins,
         matkl         TYPE mara-matkl,
         qtde_nf       TYPE j_1bnflin-menge,
         qtde_vinc     TYPE j_1bnflin-menge,
         qtde_baixa    TYPE j_1bnflin-menge,
         baixar        TYPE zsdt0276-baixar,
         valor_pago    TYPE zsdt0276-valor,
         dt_baixa      TYPE zsdt0276-dt_baixa,
         obs_icon      TYPE c LENGTH 4,
         observacao    TYPE zsdt0276-observacao,
         anexos_geral  TYPE c LENGTH 4,
         anexos_espec  TYPE c LENGTH 4,
         justificativa TYPE zsdt0276-justificativa,
         usuario       TYPE sy-uname,
         data          TYPE sy-datum,
         hora          TYPE sy-uzeit,
         acao          TYPE zsdt0277-acao,
       END OF   ty_saida.

TYPES: BEGIN OF ty_zsdt0276.
         .INCLUDE STRUCTURE zsdt0276.
         TYPES: instid_geral TYPE	srgbtbrel-instid_a,
         instid_espec TYPE srgbtbrel-instid_a,
         observ_geral TYPE thead-tdname,
         observ_espec TYPE thead-tdname,
       END OF ty_zsdt0276.

*----------------------------------------------------------------------*
* Declaração de Tabela Interna
*----------------------------------------------------------------------*
DATA: t_saida  TYPE TABLE OF ty_saida,
      t_observ TYPE TABLE OF tline.

*----------------------------------------------------------------------*
*  ALV
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*   Declaração de Instância de Métodos
*----------------------------------------------------------------------*
DATA: v_event_receiver  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*   Declaração de Container
*----------------------------------------------------------------------**
DATA: v_container_h TYPE REF TO cl_gui_container,
      v_container_i TYPE REF TO cl_gui_container.

*----------------------------------------------------------------------*
*   Declaração de GRID
*----------------------------------------------------------------------*
DATA: v_grid     TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
*    Docking
*----------------------------------------------------------------------*
DATA: v_docking  TYPE REF TO cl_gui_docking_container,
      v_splitter TYPE REF TO cl_gui_splitter_container.

*---------------------------------------------------------------------*
*   Declaração de Variáveis
*---------------------------------------------------------------------*
DATA: v_info TYPE c.

*----------------------------------------------------------------------*
*         CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*         Definição da classe lcl_event_receiver
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object
                  e_interactive.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*  ----------------------------------------------------------------------*
*         CLASS lcl_event_receiver IMPLEMENTATION
*  ----------------------------------------------------------------------*
*         Implementação da classe lcl_event_receiver
*  ----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.
  ENDMETHOD.                    "handle_hotspot_click


  METHOD handle_user_command.

*    PERFORM zf_buscar_aprovador.

    CASE e_ucomm.
      WHEN 'APROVAR'.
        PERFORM zf_efetuar_aprovacao.
      WHEN 'REPROVAR'.
        PERFORM zf_reprovar.
      WHEN 'INFO'.
        PERFORM zf_expand_legenda.
      WHEN OTHERS.
    ENDCASE.

    CALL METHOD v_grid->refresh_table_display.

  ENDMETHOD.                    "handle_user_command

  METHOD handle_toolbar.

    PERFORM zf_adiciona_botoes_header USING e_object.
    PERFORM zf_elimina_botoes_header  USING e_object.

  ENDMETHOD.                    "handle_toolbar

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*  &---------------------------------------------------------------------*
*  &      Form  ZF_ADICIONA_BOTOES_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_OBJECT  text
*  ----------------------------------------------------------------------*
FORM zf_adiciona_botoes_header USING e_object TYPE REF TO cl_alv_event_toolbar_set.

*   Add Button
  DATA: w_toolbar  TYPE stb_button.

  "Aprovar
  CLEAR w_toolbar.
  MOVE 'APROVAR'      TO w_toolbar-function.
  MOVE icon_okay      TO w_toolbar-icon.
  MOVE '0 '           TO w_toolbar-butn_type.
  MOVE 'Aprovar'(010) TO w_toolbar-quickinfo.
  MOVE 'Aprovar'(010) TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  "Reprovar
  CLEAR w_toolbar.
  MOVE 'REPROVAR'      TO w_toolbar-function.
  MOVE icon_status     TO w_toolbar-icon.
  MOVE '0 '            TO w_toolbar-butn_type.
  MOVE 'Reprovar'(010) TO w_toolbar-quickinfo.
  MOVE 'Reprovar'(010) TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_ELIMINA_BOTOES_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_OBJECT  text
*  ----------------------------------------------------------------------*
FORM zf_elimina_botoes_header  USING  e_object TYPE REF TO cl_alv_event_toolbar_set.

*      elimina itens desnecessarios da barra do container
  DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
                                 OR function = '&MB_VARIANT'
*                                   OR FUNCTION =  '&MB_EXPORT'
                                 OR function =  '&MB_SUM'
                                 OR function =  '&MB_SUBTOT'
                                 OR function =  '&PRINT_BACK'.
ENDFORM.


*----------------------------------------------------------------------*
* Tela selecao
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs      FOR j_1bnfdoc-bukrs         MODIF ID t1 NO INTERVALS NO-EXTENSION,
s_branch     FOR j_1bnfdoc-branch        MODIF ID t1,
s_docnum     FOR j_1bnfdoc-docnum        MODIF ID t4 NO INTERVALS,
s_docdat     FOR j_1bnfdoc-docdat        MODIF ID t4,
s_dtbaix     FOR zsdt0276-dt_baixa       MODIF ID t3,
s_lifnr      FOR j_1bnfdoc-parid         MODIF ID t4 NO INTERVALS,
s_matnr      FOR j_1bnflin-matnr         MODIF ID t4 NO INTERVALS,
s_status     FOR zsdt0276-status         MODIF ID t2 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END   OF BLOCK b2.

*----------------------------------------------------------------------*
* Tratar tela
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1   = 'T2' OR
       screen-group1   = 'T4'.
      screen-active    = 0.
      screen-input     = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
*START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM zf_aprovar_reprovar.

*----------------------------------------------------------------------*
*START-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF t_saida[] IS NOT INITIAL.
    CALL SCREEN 9000.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  ZF_APROVAR_REPROVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_aprovar_reprovar .

  DATA: l_msg TYPE bapiret2-message.

  "Buscar aprovadores cadastrados
  SELECT * FROM zsdt0278
    INTO TABLE @DATA(t_zsdt0278)
    WHERE bukrs IN @s_bukrs
     AND bname  =  @sy-uname.

  IF sy-subrc IS INITIAL.
    PERFORM zf_dados_adicinais_aprovar.
  ELSE.
    CONCATENATE sy-uname ',você não tem acesso para essa opção, procure a Área Execução Corporativa'
    INTO l_msg SEPARATED BY space.
    MESSAGE  l_msg TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DADOS_ADICINAIS_APROVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_dados_adicinais_aprovar .

  DATA: r_vbeln	           TYPE RANGE OF zdoc_nf_produtor-vbeln,
        r_docnum_prod	     TYPE RANGE OF zdoc_nf_produtor-docnum_prod,
        r_itmnum_prod	     TYPE RANGE OF zdoc_nf_produtor-itmnum_prod,
        r_grp_retorno	     TYPE RANGE OF zdoc_nf_produtor-grp_retorno,

        r_id_baixa         TYPE RANGE OF zsdt0276-id_baixa,
        r_docnum           TYPE RANGE OF zsdt0276-docnum,
        r_itmnum           TYPE RANGE OF zsdt0276-itmnum,
        r_status           TYPE RANGE OF zsdt0276-status,

        r_data             TYPE RANGE OF zsdt0277-data,
        r_hora             TYPE RANGE OF zsdt0277-hora,

        r_matkl            TYPE RANGE OF mara-matkl,

        r_id_nomeacao_tran TYPE RANGE OF znom_reme_notas-id_nomeacao_tran,
        r_id_empresa       TYPE RANGE OF znom_reme_notas-id_empresa,
        r_id_filial        TYPE RANGE OF znom_reme_notas-id_filial,
        r_id_material      TYPE RANGE OF znom_reme_notas-id_material,
        r_id_remetente     TYPE RANGE OF znom_reme_notas-id_remetente.

  DATA: l_msg   TYPE bapiret2-message,
        l_menge TYPE j_1bnflin-menge.

  DATA t_zsdt0276 TYPE TABLE OF ty_zsdt0276.

  DATA: w_header    TYPE thead,
        w_srgbtbrel TYPE srgbtbrel,
        w_saida     LIKE LINE OF t_saida.

  REFRESH: r_matkl.
  r_matkl = VALUE #( ( sign = 'I' option = 'EQ' low = c_const-matkl_algodao ) ).

  REFRESH: t_saida.

  "Baixa de Notas Fiscais de Compra
  REFRESH: t_zsdt0276.
  SELECT * FROM zsdt0276
           INTO TABLE t_zsdt0276
           WHERE id_baixa IN r_id_baixa
             AND docnum   IN s_docnum
             AND itmnum   IN r_itmnum
             AND dt_baixa IN s_dtbaix
             AND status   = abap_false.

  IF sy-subrc IS INITIAL.
    "Item da nota fiscal
    SELECT docnum, itmnum, matnr, meins, menge FROM j_1bnflin
      INTO TABLE @DATA(t_lin)
      FOR ALL ENTRIES IN @t_zsdt0276
      WHERE docnum = @t_zsdt0276-docnum
        AND itmnum = @t_zsdt0276-itmnum.
  ELSE.
    CONCATENATE sy-uname ', com base nas empresas que você é aprovador,'
                   'não há nenhum Baixa de NF pendente de análise.' INTO l_msg SEPARATED BY space.
    MESSAGE  l_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "LOG Ações  - Baixa de Notas Fiscais de Compra
  IF t_zsdt0276[] IS NOT INITIAL.
    SELECT * FROM zsdt0277
      INTO TABLE @DATA(t_zsdt0277)
      FOR ALL ENTRIES IN @t_zsdt0276
      WHERE id_baixa = @t_zsdt0276-id_baixa
       AND DOCNUM    = @t_zsdt0276-docnum
       AND acao      = @abap_false.
  ENDIF.

  IF t_lin[] IS NOT INITIAL.

    "Cabeçalho do documento fiscal
    SELECT docnum , bukrs, branch, nfnum, nfenum, nfe, docdat, credat, partyp, parid
        FROM j_1bnfdoc
      INTO TABLE @DATA(t_doc)
      FOR ALL ENTRIES IN @t_lin
      WHERE docnum = @t_lin-docnum
        AND bukrs  IN @s_bukrs
        AND branch IN @s_branch.

  ENDIF.

  IF t_doc[] IS NOT INITIAL.

    SELECT lifnr, name1, regio FROM lfa1
    INTO TABLE @DATA(t_lfa1)
    FOR ALL ENTRIES IN @t_doc
    WHERE lifnr = @t_doc-parid.

  ELSE.
    CONCATENATE sy-uname ', com base nos filtros informados não há nenhum Baixa de Volume de NF.'
                                                                     INTO l_msg SEPARATED BY space.
    MESSAGE  l_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF t_lin[] IS NOT INITIAL.

    DATA(t_lin_aux) = t_lin.

    SORT t_lin BY matnr.
    DELETE ADJACENT DUPLICATES FROM t_lin COMPARING ALL FIELDS.

    "Cód. de Material
    SELECT MATNR , MATKL FROM MARA
 INTO TABLE @DATA(T_MARA)
 FOR ALL ENTRIES IN @T_LIN_AUX
 WHERE MATNR = @T_LIN_AUX-MATNR
 ORDER BY PRIMARY KEY .

  ENDIF.

  IF t_mara[] IS NOT INITIAL.

    "Descrição de Material
    SELECT matnr, maktx FROM makt
      INTO TABLE @DATA(t_makt)
      FOR ALL ENTRIES IN @t_mara
      WHERE matnr = @t_mara-matnr
        AND spras = @sy-langu.

  ENDIF.

  IF t_doc[] IS NOT INITIAL.

    "Cód. dados do cliente
    SELECT kunnr, name1, regio FROM kna1
      INTO TABLE @DATA(t_kna1)
      FOR ALL ENTRIES IN @t_doc
      WHERE kunnr = @t_doc-parid.
  ENDIF.

  "Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  IF t_lin[] IS NOT INITIAL.

    SELECT * FROM zdoc_nf_produtor
      INTO TABLE @DATA(t_zdoc_nf_produtor)
      FOR ALL ENTRIES IN @t_lin
      WHERE vbeln	IN @r_vbeln
        AND docnum_prod	= @t_lin-docnum
        AND itmnum_prod	= @t_lin-itmnum
        AND grp_retorno	IN @r_grp_retorno.

  ENDIF.

  "Tabela de Notas Fiscais Remetentes Nomeações de Transporte
  IF t_doc[] IS NOT INITIAL.

    SELECT id_nomeacao_tran, id_empresa, id_filial, id_material,
           id_remetente, docnum, itmnum, grp_retorno, nr_quantidade
      FROM znom_reme_notas
      INTO TABLE @DATA(t_znom_reme_notas)
      FOR ALL ENTRIES IN @t_lin
      WHERE id_nomeacao_tran IN @r_id_nomeacao_tran
        AND id_empresa   IN @s_bukrs
        AND id_filial    IN @s_branch
        AND id_material  IN @r_id_material
        AND id_remetente IN @r_id_remetente
        AND docnum       = @t_lin-docnum
        AND itmnum       = @t_lin-itmnum
        AND grp_retorno  IN @r_grp_retorno .

  ENDIF.

  IF t_zsdt0276[] IS NOT INITIAL.

    "Preencher campos para buscar anexos
    LOOP AT t_zsdt0276 ASSIGNING FIELD-SYMBOL(<fs_zsdt0276>).
      <fs_zsdt0276>-instid_geral = <fs_zsdt0276>-id_baixa.
      <fs_zsdt0276>-instid_espec = <fs_zsdt0276>-docnum.

      CONCATENATE <fs_zsdt0276>-id_baixa '0000000000' '000000'         INTO <fs_zsdt0276>-observ_geral.
      CONCATENATE <fs_zsdt0276>-id_baixa <fs_zsdt0276>-docnum <fs_zsdt0276>-itmnum INTO <fs_zsdt0276>-observ_espec.
    ENDLOOP.

    "Busca textos com base no ID Baixa
    CLEAR: w_header.
    w_header-tdobject = 'ZOBSERVAC2'.
    w_header-tdid     = 'BXNF'.
    w_header-tdspras  = sy-langu.

    SELECT tdobject, tdname, tdid, tdspras FROM stxh
      INTO TABLE @DATA(t_stxh_geral)
      FOR ALL ENTRIES IN @t_zsdt0276
        WHERE tdobject   = @w_header-tdobject
        AND tdname       = @t_zsdt0276-observ_geral
        AND tdid         = @w_header-tdid
        AND tdspras      = @w_header-tdspras .

    "Buscar Anexos Geral
    w_srgbtbrel-reltype  = 'ATTA'.
    w_srgbtbrel-typeid_a = 'ZS_BAIXANF'.

    SELECT * FROM srgbtbrel
      INTO TABLE @DATA(t_anexos_geral)
       FOR ALL ENTRIES IN @t_zsdt0276
       WHERE reltype   = @w_srgbtbrel-reltype
         AND instid_a  = @t_zsdt0276-observ_geral
         AND typeid_a  = @w_srgbtbrel-typeid_a.

  ENDIF.

  IF t_zsdt0276[] IS NOT INITIAL.

    CLEAR: w_header.
    w_header-tdobject = 'ZOBSERVAC2'.
    w_header-tdid     = 'BXNF'.
    w_header-tdspras  = sy-langu.

    SELECT tdobject, tdname, tdid, tdspras FROM stxh
      INTO TABLE @DATA(t_stxh_espec)
      FOR ALL ENTRIES IN @t_zsdt0276
        WHERE tdobject   = @w_header-tdobject
        AND tdname       = @t_zsdt0276-observ_espec
        AND tdid         = @w_header-tdid
        AND tdspras      = @w_header-tdspras .

    "Buscar Anexos Especificos
    SELECT * FROM srgbtbrel
      INTO TABLE @DATA(t_anexos_espec)
       FOR ALL ENTRIES IN @t_zsdt0276
       WHERE reltype   =  @w_srgbtbrel-reltype
         AND instid_a  = @t_zsdt0276-observ_espec
         AND typeid_a  = @w_srgbtbrel-typeid_a.

  ENDIF.

*----------------------------------------------------------------------*
* Declaração de Estrutura
*----------------------------------------------------------------------*

  SORT: t_doc  BY docnum parid,
        t_lin  BY docnum itmnum,
        t_lfa1 BY lifnr,
        t_kna1 BY kunnr.

  LOOP AT t_zsdt0276 INTO DATA(w_zsdt0276).

    w_saida-id_baixa = w_zsdt0276-id_baixa.

    w_saida-docnum   = w_zsdt0276-docnum.

    w_saida-itmnum   = w_zsdt0276-itmnum.

    READ TABLE t_doc INTO DATA(w_doc) WITH KEY docnum = w_saida-docnum BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    w_saida-bukrs    = w_doc-bukrs.

    w_saida-branch   = w_doc-branch.

    w_saida-nfe      = w_doc-nfe.

    "NRO. NF
    IF w_doc-nfe IS NOT INITIAL.

      w_saida-nfenum  = w_doc-nfenum.

    ELSE.

      w_saida-nfenum  = w_doc-nfnum.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_saida-nfenum
        IMPORTING
          output = w_saida-nfenum.

    ENDIF.

    "DT. CRIAÇÃO
    w_saida-credat = w_doc-credat.

    "DT. EMISSÃO
    w_saida-docdat = w_doc-docdat.

    w_saida-partyp = w_doc-partyp.

    "Tipo do parceiro
    CASE w_doc-partyp.
      WHEN 'V'.
        READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = w_doc-parid BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida-parid = w_lfa1-lifnr.
          w_saida-name1 = w_lfa1-name1.
          w_saida-regio = w_lfa1-regio.
        ENDIF.
      WHEN 'C'.
        READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY kunnr = w_doc-parid BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida-parid = w_kna1-kunnr.
          w_saida-name1 = w_kna1-name1.
          w_saida-regio = w_kna1-regio.
        ENDIF.
      WHEN 'B'.
        READ TABLE t_lfa1 INTO w_lfa1 WITH KEY lifnr = w_doc-parid BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida-parid = w_lfa1-lifnr.
          w_saida-name1 = w_lfa1-name1.
          w_saida-regio = w_lfa1-regio.
        ENDIF.
    ENDCASE.

    READ TABLE t_lin INTO DATA(w_lin) WITH KEY docnum = w_zsdt0276-docnum
                                               itmnum = w_zsdt0276-itmnum BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    "Cód. do mateiral
    READ TABLE t_mara INTO DATA(w_mara) WITH KEY matnr = w_lin-matnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      w_saida-matnr = w_mara-matnr.
      w_saida-matkl = w_mara-matkl.
    ENDIF.

    "Descrição do material
    READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_lin-matnr.
    IF sy-subrc EQ 0.

      w_saida-maktx = w_makt-maktx.

    ENDIF.

    w_saida-meins = w_lin-meins.

    "QTDE. NF (kg)
    IF w_lin-meins NE 'KG'.

      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = w_lin-matnr
          i_mein1             = w_lin-meins
          i_meins             = 'KG'
          i_menge             = w_lin-menge
        IMPORTING
          menge               = w_saida-qtde_nf
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        w_saida-qtde_nf = w_lin-menge.
      ENDIF.
    ELSE.
      w_saida-qtde_nf = w_lin-menge.
    ENDIF.

    "QTDE. VINCULADA
    IF w_mara-matkl IN r_matkl.

      LOOP AT t_zdoc_nf_produtor INTO DATA(w_nf_produtor) WHERE docnum_prod EQ w_saida-docnum.
        ADD w_nf_produtor-menge TO w_saida-qtde_vinc.
      ENDLOOP.

    ELSE.

      LOOP AT t_znom_reme_notas INTO DATA(w_znom_reme_notas) WHERE docnum EQ w_saida-docnum.
        ADD w_znom_reme_notas-nr_quantidade TO w_saida-qtde_vinc.
      ENDLOOP.

    ENDIF.

    w_saida-qtde_baixa  = w_zsdt0276-menge.

    w_saida-baixar       = w_zsdt0276-baixar.

    w_saida-valor_pago   = w_zsdt0276-valor.

    w_saida-dt_baixa     = w_zsdt0276-dt_baixa.

    "Verifica existência de texto id_baida + docnum + itmnum
    READ TABLE t_stxh_espec INTO DATA(w_stxh_espec) WITH KEY tdname = w_zsdt0276-observ_espec.
    IF sy-subrc IS INITIAL.
      w_saida-obs_icon      = icon_display_text.
    ENDIF.

    "Verifica existência de texto nívell id_baixa
    READ TABLE t_stxh_geral INTO DATA(w_stxh_geral) WITH KEY tdname = w_zsdt0276-observ_geral.
    IF sy-subrc IS INITIAL.
      w_saida-obs_icon      = icon_display_text.
    ENDIF.

    "Verifica existência de anexos  id_baida + docnum + itmnum
    READ TABLE t_anexos_geral INTO DATA(w_anexos_geral) WITH KEY instid_a = w_zsdt0276-observ_geral.
    IF sy-subrc IS INITIAL.
      w_saida-anexos_geral = icon_attachment.
    ENDIF.

    "Verifica existência de anexos  id_baida
    READ TABLE t_anexos_espec INTO DATA(w_anexos_espec) WITH KEY instid_a = w_zsdt0276-observ_espec.
    IF sy-subrc IS INITIAL.
      w_saida-anexos_espec = icon_attachment.
    ENDIF.

    READ TABLE t_zsdt0277 INTO DATA(w_zsdt0277) WITH KEY id_baixa = w_saida-id_baixa
                                                           acao   = abap_false.
    IF sy-subrc IS INITIAL.
      w_saida-usuario  = w_zsdt0277-usuario.
      w_saida-data     = w_zsdt0277-data.
      w_saida-hora     = w_zsdt0277-hora.
    ENDIF.

    APPEND w_saida TO t_saida.
    CLEAR w_saida.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM zf_handle_hotspot_click    USING    p_index  TYPE any
                                         p_column TYPE any.

  DATA: w_bor    TYPE borident.

  READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX p_index.

  CHECK sy-subrc IS INITIAL.

  CASE p_column.
    WHEN 'DOCNUM'.
      PERFORM zf_j1b3n USING <fs_saida>.
    WHEN 'OBS_ICON'.
      PERFORM zf_popup_obs USING abap_true CHANGING <fs_saida>.
    WHEN 'ANEXOS_GERAL'.
      IF <fs_saida>-anexos_geral IS NOT INITIAL.
        CLEAR w_bor.
        CONCATENATE <fs_saida>-id_baixa '0000000000' '000000'  INTO w_bor-objkey.
        w_bor-objtype = 'ZS_BAIXANF'.
        PERFORM zf_chamar_anexo USING 'D' CHANGING w_bor <fs_saida>-anexos_geral.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_status_9000 OUTPUT.

  SET PF-STATUS 'ZGPP_STATUS_9000'.
  SET TITLEBAR  'ZUPP_TITULO'.

  PERFORM zf_split_screen.
  PERFORM zf_preparar_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_SPLIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_split_screen .


  CLEAR: v_docking, v_splitter, v_container_h, v_container_i.
  CREATE OBJECT v_docking
    EXPORTING
      repid = sy-repid
      dynnr = sy-dynnr
      extension = 9999.
*      ratio = '95'.

*   Create a splitter with 2 rows and 1 column
  CREATE OBJECT v_splitter
    EXPORTING
      parent  = v_docking
      rows    = 1
      columns = 1.

*  * Lower Container
  CALL METHOD v_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = v_container_i.

*  * Upper Container height
  CALL METHOD v_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_header .


  DATA: l_document  TYPE REF TO cl_dd_document,
        l_doctable  TYPE REF TO cl_dd_table_element,
        l_column1   TYPE REF TO cl_dd_area,
        l_column2   TYPE REF TO cl_dd_area,
        l_text(255) TYPE c.  "Text

  CREATE OBJECT l_document.
*  ************ - Titulo Legenda
  CLEAR l_text.
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_text
    EXPORTING
      text         = 'Legenda'(023)
      sap_emphasis = 'STRONG'.

  CALL METHOD l_document->new_line.

*  *************L4 - Email enviado com sucesso
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_SYSTEM_OKAY'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Email enviado com sucesso'(026).

  CALL METHOD l_document->new_line.

*  *************L5 - Erro no envio do mail
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_SYSTEM_CANCEL'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Erro envio de Email'(027).

  CALL METHOD l_document->new_line.

*  ********************************************
  CALL METHOD l_document->display_document
    EXPORTING
      parent = v_container_h.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_alv .


  DATA: t_fcat TYPE lvc_t_fcat,
        t_sort TYPE lvc_t_sort.

  DATA: w_variant TYPE disvariant,
        w_layout  TYPE lvc_s_layo.

  w_layout-cwidth_opt = 'X'.
  w_layout-zebra      = 'X'.
  w_layout-sel_mode   = 'A'.

  w_variant-report    = sy-repid.


  IF v_grid IS INITIAL.

    PERFORM zf_montar_fieldcat CHANGING t_saida t_fcat.
    PERFORM zf_ajuste_fieldcat CHANGING t_fcat.

    CREATE OBJECT v_grid
      EXPORTING
        i_parent          = v_container_i
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT v_event_receiver.
    SET HANDLER v_event_receiver->handle_hotspot_click FOR v_grid.
    SET HANDLER v_event_receiver->handle_toolbar       FOR v_grid.
    SET HANDLER v_event_receiver->handle_user_command  FOR v_grid.

    CALL METHOD v_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = w_variant
        i_save                        = 'A'
        is_layout                     = w_layout
      CHANGING
        it_fieldcatalog               = t_fcat
        it_outtab                     = t_saida
        it_sort                       = t_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD v_grid->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat CHANGING pt_tabela   TYPE ANY TABLE
                                    pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

*   Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*   Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

*   Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

*   Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_ajuste_fieldcat  CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

    <fs_fcat>-coltext = <fs_fcat>-reptext.

    CASE <fs_fcat>-fieldname.
      WHEN 'ID_BAIXA'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'ID'.
      WHEN 'BUKRS'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Empresa'.
      WHEN 'BRANCH'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Centro'.
      WHEN 'DOCNUM'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Docnum'.
        <fs_fcat>-hotspot = abap_true.
      WHEN 'ITMNUM'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Item'.
      WHEN 'NFE'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'NFENUM'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Nro. NF'.
      WHEN 'CREDAT'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Dt. Criação'.
      WHEN 'DOCDAT'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Dt. Emissão'.
      WHEN 'PARTYP'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'PARID'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Parceiro'.
      WHEN 'NAME1'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Nome Parceiro'.
      WHEN 'REGIO'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Estado'.
      WHEN 'MATNR'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Material'.
      WHEN 'MAKTX'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Desc. Material'.
      WHEN 'MEINS'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'MATKL'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'QTDE_NF'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Qtde. NF (kg)'.
      WHEN 'QTDE_VINC'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Qtde. Vinculada'.
      WHEN 'QTDE_BAIXA'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Qtde. Baixada'.
      WHEN 'BAIXAR'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Baixar Volume ?'.
        <fs_fcat>-checkbox = abap_true.
      WHEN 'VALOR_PAGO'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Valor Pago'.
      WHEN 'DT_BAIXA'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Data Baixa'.
      WHEN 'OBS_ICON'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Observações'.
        <fs_fcat>-just = 'C'.
        <fs_fcat>-hotspot = abap_true.
      WHEN 'OBSERVACAO'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'ANEXOS_GERAL'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Anexos(Geral)'.
        <fs_fcat>-just = 'C'.
        <fs_fcat>-hotspot = abap_true.
      WHEN 'ANEXOS_ESPEC'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'USUARIO'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Usuário'.
      WHEN 'DATA'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Data'.
      WHEN 'HORA'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Hora'.
      WHEN 'JUSTIFICATIVA'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN 'ACAO'.
        <fs_fcat>-no_out = abap_true.
        <fs_fcat>-tech   = abap_true.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_POPUP_JUST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_popup_just USING p_edit CHANGING p_saida TYPE ty_saida.

  DATA : t_txline TYPE STANDARD TABLE OF txline,
         w_txline LIKE LINE OF t_txline.

  CLEAR w_txline.

  w_txline = p_saida-justificativa.
  APPEND w_txline TO t_txline.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = 'Motivo da Reprovação'
      im_display_mode = p_edit
      im_start_column = 10
      im_start_row    = 5
      im_no_toolbar   = 'X'
    CHANGING
      ch_text         = t_txline.

  IF t_txline[] IS NOT INITIAL.
    CLEAR w_txline.
    LOOP AT t_txline INTO w_txline.
      CONCATENATE p_saida-justificativa w_txline INTO p_saida-justificativa.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_J1B3N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_SAIDA>  text
*----------------------------------------------------------------------*
FORM zf_j1b3n  USING p_saida TYPE ty_saida.
  SET PARAMETER ID 'JEF' FIELD p_saida-docnum.
  CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXPAND_LEGENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_expand_legenda .

  IF v_info IS INITIAL.
    CALL METHOD v_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 15.

    v_info = 'X'.
  ELSE.
    CALL METHOD v_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 0.

    CLEAR v_info.
  ENDIF.

  CALL METHOD v_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EFETUAR_APROVACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_efetuar_aprovacao .

  DATA: t_index_rows   TYPE lvc_t_row,                      "#EC NEEDED
        t_row_no       TYPE lvc_t_roid,
        t_zsdt0277_upd TYPE TABLE OF zsdt0277,

        w_row_no       LIKE LINE OF t_row_no,
        w_saida        LIKE LINE OF t_saida,
        w_zsdt0277_upd LIKE LINE OF t_zsdt0277_upd,

        l_linhas       TYPE sy-tabix,
        l_answer       TYPE c.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "Abrir popup preenchimento de justificativa
  CLEAR w_saida.
  REFRESH: t_zsdt0277_upd.

  DATA(t_saida_aux) = t_saida.

  LOOP AT t_row_no INTO w_row_no.

    CLEAR w_zsdt0277_upd.
    READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.
    IF sy-subrc IS INITIAL.

      <fs_saida>-acao = 'A'.

      w_zsdt0277_upd-id_baixa = <fs_saida>-id_baixa.
      w_zsdt0277_upd-docnum   = <fs_saida>-docnum.
      w_zsdt0277_upd-itmnum   = <fs_saida>-itmnum.
      w_zsdt0277_upd-data     = sy-datum.
      w_zsdt0277_upd-hora     = sy-uzeit.
      w_zsdt0277_upd-acao     = 'A'.
      w_zsdt0277_upd-usuario  = sy-uname.
      APPEND w_zsdt0277_upd TO t_zsdt0277_upd.
    ENDIF.
  ENDLOOP.

  IF t_zsdt0277_upd[] IS NOT INITIAL.

    MODIFY zsdt0277 FROM TABLE t_zsdt0277_upd.

    SELECT * FROM zsdt0276
       INTO TABLE @DATA(t_zsdt0276_upd)
       FOR ALL ENTRIES IN @t_zsdt0277_upd
        WHERE id_baixa = @t_zsdt0277_upd-id_baixa
          AND docnum   = @t_zsdt0277_upd-docnum
          AND itmnum   = @t_zsdt0277_upd-itmnum.

    LOOP AT t_zsdt0276_upd ASSIGNING FIELD-SYMBOL(<fs_zsdt0276_upd>).
      <fs_zsdt0276_upd>-status        = 'A'.
    ENDLOOP.

    IF sy-subrc IS INITIAL.
      MODIFY zsdt0276 FROM TABLE t_zsdt0276_upd.
    ENDIF.
    COMMIT WORK AND WAIT.

    DELETE t_saida WHERE acao = 'A'.

    MESSAGE s000(zppr) WITH 'Documento(s) aprovado(s) com sucesso(s)!' DISPLAY LIKE 'S'.
    RETURN.

  ENDIF.

  CALL METHOD v_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_REPROVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_reprovar .

  DATA: t_index_rows   TYPE lvc_t_row,                      "#EC NEEDED
        t_row_no       TYPE lvc_t_roid,
        t_zsdt0277_upd TYPE TABLE OF zsdt0277,
        t_saida_aux    TYPE TABLE OF zstsd_email_baixas,

        w_row_no       LIKE LINE OF t_row_no,
        w_saida        LIKE LINE OF t_saida,
        w_saida_aux    LIKE LINE OF t_saida_aux,
        w_zsdt0277_upd LIKE LINE OF t_zsdt0277_upd,

        l_linhas       TYPE sy-tabix,
        l_answer       TYPE c.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "Abrir popup preenchimento de justificativa
  CLEAR w_saida.
  REFRESH: t_zsdt0277_upd.

  REFRESH:  t_observ.
  PERFORM zf_popup_just_reprova USING abap_false CHANGING w_saida.

  IF  t_observ IS NOT INITIAL.

    REFRESH: t_saida_aux.
    LOOP AT t_row_no INTO w_row_no.

      CLEAR w_zsdt0277_upd.
      READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

      IF sy-subrc IS INITIAL.

        <fs_saida>-acao = 'R'.

        w_zsdt0277_upd-id_baixa = <fs_saida>-id_baixa.
        w_zsdt0277_upd-docnum   = <fs_saida>-docnum.
        w_zsdt0277_upd-itmnum   = <fs_saida>-itmnum.
        w_zsdt0277_upd-data     = sy-datum.
        w_zsdt0277_upd-hora     = sy-uzeit.
        w_zsdt0277_upd-acao     = 'R'.
        w_zsdt0277_upd-usuario  = sy-uname.

        APPEND w_zsdt0277_upd TO t_zsdt0277_upd.

        MOVE-CORRESPONDING <fs_saida> TO w_saida_aux.
        APPEND w_saida_aux TO t_saida_aux.

      ENDIF.
    ENDLOOP.

    IF t_zsdt0277_upd[] IS NOT INITIAL.

      MODIFY zsdt0277 FROM TABLE t_zsdt0277_upd.

      "Buscar linhas da baixa
      SELECT * FROM zsdt0276
         INTO TABLE @DATA(t_zsdt0276_upd)
         FOR ALL ENTRIES IN @t_zsdt0277_upd
          WHERE id_baixa = @t_zsdt0277_upd-id_baixa
            AND docnum   = @t_zsdt0277_upd-docnum
            AND itmnum   = @t_zsdt0277_upd-itmnum.

      LOOP AT t_zsdt0276_upd ASSIGNING FIELD-SYMBOL(<fs_zsdt0276_upd>).

        <fs_zsdt0276_upd>-status        = 'R'.
        <fs_zsdt0276_upd>-justificativa = w_saida-justificativa.

        "Grava texto da reprova
        PERFORM zf_popup_just_reprova_save USING
                                            <fs_zsdt0276_upd>-id_baixa
                                            <fs_zsdt0276_upd>-docnum
                                            <fs_zsdt0276_upd>-itmnum
                                            'R'.
      ENDLOOP.

      IF t_zsdt0276_upd IS NOT INITIAL.
        MODIFY zsdt0276 FROM TABLE t_zsdt0276_upd.
      ENDIF.

      COMMIT WORK AND WAIT.
*---------------------------------------------------------------------*
* Envio de E-mail
*---------------------------------------------------------------------*
      DELETE t_saida WHERE acao ='R'.

      IF t_saida_aux[] IS NOT INITIAL.

        "Envia e-mail
        CALL FUNCTION 'ZSD_EMAIL_BAIXAS'
          TABLES
            t_baixas = t_saida_aux.

      ENDIF.

      MESSAGE s000(zppr) WITH 'Documento(s) reprovado(s) com sucesso(s)!' DISPLAY LIKE 'S'.
      RETURN.

    ENDIF.

  ELSE.
    MESSAGE s000(zppr) WITH 'Justificativa de reprovação não informada!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL METHOD v_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_POPUP_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_SAIDA>  text
*----------------------------------------------------------------------*
FORM zf_popup_obs USING p_edit CHANGING p_saida TYPE ty_saida.

  DATA: t_tline     TYPE STANDARD TABLE OF tline,
        w_tline     LIKE LINE OF t_tline,

        t_texto     TYPE catsxt_longtext_itab,
        w_texto     LIKE LINE OF t_texto,

        l_name_text TYPE thead-tdname,
        l_chave     TYPE char26,

        w_header    TYPE thead.


  CONCATENATE p_saida-id_baixa '0000000000' '000000' INTO l_chave.

  l_name_text       = l_chave.
  w_header-tdobject = 'ZOBSERVAC2'.
  w_header-tdname   = l_name_text.
  w_header-tdid     = 'BXNF'.
  w_header-tdspras  = sy-langu.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id        = w_header-tdid
      language  = w_header-tdspras
      name      = l_name_text
      object    = w_header-tdobject
    TABLES
      lines     = t_tline
    EXCEPTIONS
      id        = 1
      language  = 2
      name      = 3
      not_found = 4
      OTHERS    = 5.

  IF sy-subrc IS INITIAL.
    LOOP AT t_tline INTO w_tline.
      MOVE: w_tline-tdline TO w_texto.
      APPEND w_texto TO t_texto.
      CLEAR: w_texto.
    ENDLOOP.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = 'Observação'
        im_display_mode = p_edit
        im_start_column = 10
        im_start_row    = 10
        im_no_toolbar   = abap_true
      CHANGING
        ch_text         = t_texto.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_POPUP_JUST_REPROVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ABAP_FALSE  text
*      <--P_W_SAIDA  text
*----------------------------------------------------------------------*
FORM zf_popup_just_reprova  USING p_edit CHANGING p_saida TYPE ty_saida.

  DATA :
    t_txline TYPE STANDARD TABLE OF txline,
    w_txline LIKE LINE OF t_txline,

    w_observ LIKE LINE  OF t_observ.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = 'Motivo da Reprovação'
      im_display_mode = p_edit
      im_start_column = 10
      im_start_row    = 5
      im_no_toolbar   = 'X'
    CHANGING
      ch_text         = t_txline.

  IF t_txline[] IS NOT INITIAL.

    LOOP AT t_txline INTO w_txline.
      w_observ-tdformat = '*'.
      w_observ-tdline   = w_txline.
      APPEND w_observ  TO t_observ.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CHAMA_ANEXO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ABAP_TRUE  text
*      <--P_<FS_SAIDA>  text
*----------------------------------------------------------------------*
FORM zf_chamar_anexo USING p_mode CHANGING p_bor TYPE borident
                                           p_anexos.

  DATA: lcl_anexo_obj TYPE REF TO cl_gos_manager,
        l_ip_service  TYPE sgs_srvnam,
        l_lines       TYPE sy-tfill.

  CREATE OBJECT lcl_anexo_obj TYPE cl_gos_manager.

  lcl_anexo_obj->set_rw_mode( ip_mode = p_mode ).

  lcl_anexo_obj->start_service_direct(
    EXPORTING
      ip_service         = 'VIEW_ATTA' "l_ip_service
      is_object          = p_bor
    EXCEPTIONS
      no_object          = 1
      object_invalid     = 2
      execution_failed   = 3
      OTHERS             = 4 ).

  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_POPUP_JUST_REPROVA_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ABAP_FALSE  text
*      <--P_W_SAIDA  text
*----------------------------------------------------------------------*
FORM zf_popup_just_reprova_save USING p_id_baixa TYPE zsdt0276-id_baixa
                                      p_docnum TYPE zsdt0276-docnum
                                      p_itmnum TYPE zsdt0276-itmnum
                                      p_tipo   TYPE c.

  DATA: w_header TYPE thead,
        l_chave  TYPE char26.

  CLEAR: w_header, l_chave.

  w_header-tdobject = 'ZOBSERVAC2'.
  CONCATENATE p_id_baixa p_docnum p_itmnum p_tipo INTO w_header-tdname.

  w_header-tdid     = 'BXNF'.
  w_header-tdspras  = sy-langu.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      client          = sy-mandt
      header          = w_header
*     INSERT          = ' '
      savemode_direct = 'X'
    TABLES
      lines           = t_observ
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

ENDFORM.
