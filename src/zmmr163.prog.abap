*&---------------------------------------------------------------------*
*& Report ZMMR163
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Relatório de divergência e Rateios - Frete Ferroviários *
* Transação..:                                                         *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
**26.10.20|JALEXANDRE |         | Desenvolvimento Inicial              *
*----------------------------------------------------------------------*
REPORT zmmr163.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
TABLES: lfa1, bkpf, j_1bbranch, j_1bnfdoc,
        vttk, zib_cte_dist_ter, zib_cte_dist_n55,
        mara, dd02d.

*----------------------------------------------------------------------*
* Declaração de Ranges
*----------------------------------------------------------------------*
DATA: r_bukrs TYPE RANGE OF t001-bukrs.

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF y_diverg,
         status(20)       TYPE c, "icon_green_light ou icon_red_light
         e_tomadora       TYPE zib_cte_dist_ter-e_tomadora,
         f_tomadora       TYPE zib_cte_dist_ter-f_tomadora,
         f_name1          TYPE t001w-name1,
         dt_emissao       TYPE zib_cte_dist_ter-dt_emissao,
         p_emissor        TYPE zib_cte_dist_ter-p_emissor,
         p_emi_name1      TYPE lfa1-name1,
         numr_cte         TYPE zib_cte_dist_ter-numr_cte,
         numr_serie       TYPE zib_cte_dist_ter-numr_serie,
         tarifa_cobrada   TYPE zib_cte_dist_cvl-valr_componente,
         qt_carga_cte     TYPE zib_cte_dist_ter-qt_carga_cte,
         city1            TYPE adrc-city1,
         taxjurcode       TYPE adrc-city1,
         valor_prestacao  TYPE zib_cte_dist_ter-valor_prestacao,
         zmatnr_merc      TYPE zib_cte_dist_nit-zmatnr_merc,
         maktx            TYPE makt-maktx,
         matkl            TYPE mara-matkl,
         wgbez60          TYPE t023t-wgbez60,
         muni_coleta(63)  TYPE c,
         muni_entrega(63) TYPE c,
         kbetr            TYPE konv-kbetr,
         cod_exp          TYPE lfa1-lifnr,
         expedidor(20)    TYPE c,
         exped_rsocial    TYPE zib_cte_dist_ter-exped_rsocial,
         muni_exped(16)   TYPE c,
         cod_rec          TYPE lfa1-lifnr,
         recebedor(20)    TYPE c,
         receb_rsocial    TYPE zib_cte_dist_ter-receb_rsocial,
         muni_receb(16)   TYPE c,
         ponto_coleta     TYPE lfa1-lifnr,
         cd_chave_cte     TYPE  zib_cte_dist_ter-cd_chave_cte,
         peso_chegada     TYPE zib_cte_dist_ter-peso_chegada,
         dt_chegada       TYPE zib_cte_dist_ter-dt_chegada,
         tipo_tk          TYPE dd02d-ddtext,
         shtyp            TYPE vttk-shtyp,
         lzone_pc         TYPE vbpa-lzone,
         lzone_lr         TYPE vbpa-lzone,
         lzone_z1         TYPE vbpa-lzone,
         local_exped      TYPE t001w-werks,
         cod_postal       TYPE zde_cidade_base,
         data_ref         TYPE sy-datum,
         color_line(4)    TYPE c,
         color_cell       TYPE lvc_t_scol,
       END OF y_diverg.

TYPES: BEGIN OF y_rateio,
         e_tomadora       TYPE zib_cte_dist_ter-e_tomadora,
         f_tomadora       TYPE zib_cte_dist_ter-f_tomadora,
         f_name1          TYPE t001w-name1,
         emit_cnpj        TYPE lfa1-stcd1,
         p_emissor        TYPE zib_cte_dist_ter-p_emissor,
         p_emi_name1      TYPE lfa1-name1,
         numr_cte         TYPE zib_cte_dist_ter-numr_cte,
         dt_emissao       TYPE zib_cte_dist_ter-dt_emissao,
         cd_chave_cte     TYPE string, "zib_cte_dist_ter-cd_chave_cte,
         n55_chave_acesso TYPE string, "zib_cte_dist_n55-n55_chave_acesso,
         nf_propria       TYPE j_1bnfdoc-nfenum,
         nf_terceiro      TYPE j_1bnfdoc-nfenum,
*         ponto_coleta     TYPE lfa1-lifnr,
         city1            TYPE adrc-city1,
         taxjurcode       TYPE adrc-city1,
         tarifa_cobrada   TYPE zib_cte_dist_cvl-valr_componente,
         pesosaida        TYPE zlest0039-pesosaida,
         datatransb       TYPE zlest0039-datatransb,
         pesotransb       TYPE zlest0039-pesotransb,
         qtdade           TYPE zlest0039-pesotransb, "zib_cte_dist_d55-valr_peso_rate,
         saldo            TYPE zlest0039-pesotransb, "zib_cte_dist_d55-valr_peso_rate,
         tknum            TYPE zib_cte_dist_n55-tknum,
         fknum            TYPE zib_cte_dist_n55-fknum,
         ds_texto         TYPE zib_cte_dist_cpl-ds_texto,
         color_line(4)    TYPE c,
         color_cell       TYPE lvc_t_scol,
       END OF y_rateio.

TYPES: BEGIN OF y_filtros,
         parametro  TYPE string,
         valor      TYPE string,
         direita    TYPE string,
         parametro2 TYPE string,
         valor2     TYPE string,
       END OF y_filtros.

TYPES: BEGIN OF y_n55_ch,
         docnum_cte       TYPE zib_cte_dist_ter-docnum_cte,
         cd_chave_cte     TYPE zib_cte_dist_n55-cd_chave_cte,
         docnum_nfe       TYPE zib_cte_dist_n55-docnum_nfe,
         n55_chave_acesso TYPE zib_cte_dist_n55-n55_chave_acesso.
         INCLUDE STRUCTURE j_1b_nfe_access_key.
         TYPES: branch           TYPE zib_cte_dist_n55-branch,
         parid            TYPE zib_cte_dist_n55-parid,
         parvw            TYPE zib_cte_dist_n55-parvw,
         vbeln_vl         TYPE zib_cte_dist_n55-vbeln_vl,
       END OF y_n55_ch.
*----------------------------------------------------------------------*
* Tabela Interna
*----------------------------------------------------------------------*
DATA: t_filtro       TYPE TABLE OF y_filtros,
      t_saida_diverg TYPE TABLE OF y_diverg, "Tabela de saída de divergência
      t_saida_rateio TYPE TABLE OF y_rateio. "Tabela de saída de rateio

DATA:       t_coltab    TYPE lvc_t_scol.
*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: BEGIN OF gc_icones,
             icon_red(20)   TYPE c VALUE '@F1@',
             icon_green(20) TYPE c VALUE '@DF@',
           END OF gc_icones.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS       lcl_event_receiver DEFINITION FINAL.
  PUBLIC SECTION.

*---Method to handel hotspot
    METHODS :
      handle_hotspot_click
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

*---Method to handel toolbar
    METHODS :
      handle_toolbar
                  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.

*---Method to handel user_command
    METHODS :
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS .                    "LCL_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
* Declaração de Variáveis
*----------------------------------------------------------------------*
DATA: v_grid           TYPE REF TO cl_gui_alv_grid,         "#EC NEEDED
      v_grid2          TYPE REF TO cl_gui_alv_grid,         "#EC NEEDED
      v_event_receiver TYPE REF TO lcl_event_receiver,      "#EC NEEDED
      v_titulo         TYPE sy-title.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

*-----Logic to handle the HOTSPOT click
  METHOD handle_hotspot_click.
*---To handel hotspot in the firstlist
    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_toolbar.
*    PERFORM ZF_ADICIONA_BOTOES_HEADER USING E_OBJECT.
    PERFORM zf_elimina_botoes_header  USING e_object.
  ENDMETHOD.

  METHOD handle_user_command.
*    CASE E_UCOMM.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*&---------------------------------------------------------------------*
**Tela de Parâmetros 2
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS: r_div RADIOBUTTON GROUP gp1 USER-COMMAND mt DEFAULT 'X',
            r_rat RADIOBUTTON GROUP gp1.
SELECTION-SCREEN: END OF BLOCK  bl1.

*&---------------------------------------------------------------------*
*Tela de Parâmetros 1
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr         MODIF ID all,
s_stcd1  FOR lfa1-stcd1                        MODIF ID all,
s_budat  FOR bkpf-budat                        MODIF ID all,
s_bukrs  FOR bkpf-bukrs                        MODIF ID all,
s_branch FOR j_1bbranch-branch                 MODIF ID div,
s_matnr  FOR mara-matnr                        MODIF ID div,
s_matkl  FOR mara-matkl                        MODIF ID div,
s_numcte FOR zib_cte_dist_ter-numr_cte         MODIF ID all,
s_chcte  FOR zib_cte_dist_ter-cd_chave_cte     MODIF ID all,
s_numnfe FOR zib_cte_dist_ter-docnum9          MODIF ID rat ,
s_chnfe  FOR zib_cte_dist_n55-n55_chave_acesso MODIF ID rat,
s_numnft FOR zib_cte_dist_ter-docnum9          MODIF ID rat.
SELECTION-SCREEN: END OF BLOCK  bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
PARAMETERS: p_layout      TYPE disvariant-variant MODIF ID t1 DEFAULT '/STD'.
SELECTION-SCREEN END OF BLOCK bl3.

**&---------------------------------------------------------------------*
**Inicialização
**&---------------------------------------------------------------------*
*INITIALIZATION.

**&---------------------------------------------------------------------*
**Tratar campos da tela seleção
**&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM zf_monta_tela.

**&---------------------------------------------------------------------*
**Início do processamento
**&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_limpar_variaveis.
  PERFORM zf_validar_campos.

*END-OF-SELECTION.
  IF t_saida_diverg[] IS NOT INITIAL OR t_saida_rateio[] IS NOT INITIAL.
    PERFORM zf_alv.
  ENDIF.
*
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_elimina_botoes_header  USING e_object
                                     TYPE REF TO cl_alv_event_toolbar_set.

*    elimina itens desnecessarios da barra do container
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
*                                   OR function = '&MB_VARIANT'
*                                 OR FUNCTION = '&MB_EXPORT'
                                 OR function = '&PRINT_BACK'.
*                                 OR FUNCTION = '&MB_SUM'
*                                 OR FUNCTION = '&MB_SUBTOT'.

ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  ZF_LIMPAR_VARIAVEIS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM zf_limpar_variaveis .

  IF r_div IS NOT INITIAL.
    REFRESH: s_chnfe, s_numnfe, r_bukrs.
    CLEAR: r_bukrs, s_chnfe, s_numnfe.
  ENDIF.

  IF r_rat IS NOT INITIAL.
    REFRESH: r_bukrs, s_branch, s_matnr, s_matkl, r_bukrs.
    CLEAR: r_bukrs, s_branch, s_matnr, s_matkl.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv .
  CALL SCREEN 9000.
ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  ZF_BUSCAR_DADOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM zf_preparar_dados_divergencia.

  TYPES: BEGIN OF y_expedidor,
           exped_tp_doc TYPE zib_cte_dist_ter-exped_tp_doc,
           exped_cnpj   TYPE lfa1-stcd1,
           exped_cpf    TYPE lfa1-stcd2,
         END OF y_expedidor.

  TYPES: BEGIN OF y_recebedor,
           exped_tp_doc TYPE zib_cte_dist_ter-exped_tp_doc,
           receb_tp_doc TYPE zib_cte_dist_ter-receb_tp_doc,
           receb_cnpj   TYPE lfa1-stcd1,
           receb_cpf    TYPE lfa1-stcd2,
         END OF y_recebedor.

  TYPES: BEGIN OF y_lin,
           docnum TYPE j_1bnflin-docnum,
           itmnum TYPE j_1bnflin-itmnum,
           werks  TYPE j_1bnflin-werks,
           refkey TYPE vbfa-vbeln,
           reftyp TYPE j_1bnflin-reftyp,
         END OF y_lin.

  DATA: t_expedidor TYPE TABLE OF y_expedidor,
        t_recebedor TYPE TABLE OF y_recebedor,
        t_lin       TYPE TABLE OF y_lin,
        t_lin_li    TYPE TABLE OF y_lin.

  DATA: r_cd_modal TYPE RANGE OF zmodal.

  r_cd_modal = VALUE #( ( sign = 'I' option = 'EQ' low = '04' )
                         ( sign = 'I' option = 'EQ' low = '06' ) ).


  REFRESH: t_expedidor,  t_recebedor.

  SELECT cd_chave_cte, numr_cte, numr_serie, dt_emissao, valor_prestacao,
    cd_tomador, emit_cnpj, emit_rsocial, exped_tp_doc, exped_cnpj, exped_cpf, exped_rsocial,
     receb_cnpj, receb_cpf, receb_rsocial, dest_cnpj, dest_cpf, inicio_muni, inicio_uf,
     termino_muni, termino_uf, ds_prod_pred, qt_carga_cte, e_tomadora,
    f_tomadora, p_emissor, belnr, peso_chegada, dt_chegada
      FROM zib_cte_dist_ter
      INTO TABLE @DATA(t_zib_cte_dist_ter)
      WHERE cd_chave_cte   IN @s_chcte
        AND numr_cte       IN @s_numcte
            AND dt_emissao IN @s_budat
            AND emit_cnpj  IN @s_stcd1
           " AND cd_modal = '04'
           AND cd_modal IN @r_cd_modal
            AND e_tomadora IN @r_bukrs
            AND f_tomadora IN @s_branch
            AND p_emissor  IN @s_lifnr.

  IF t_zib_cte_dist_ter[] IS NOT INITIAL.

**----------------------------------------------------------------------*
    "Tabela de InBound de CT-e Distribuida - NF55
**----------------------------------------------------------------------*
    SELECT cd_chave_cte, n55_chave_acesso, docnum_nfe, branch, parvw, parid,
            tknum, fknum FROM zib_cte_dist_n55
    INTO TABLE @DATA(t_zib_cte_dist_n55)
    FOR ALL ENTRIES IN @t_zib_cte_dist_ter
    WHERE cd_chave_cte = @t_zib_cte_dist_ter-cd_chave_cte
      AND bukrs  IN @r_bukrs
      AND branch IN @s_branch.

**----------------------------------------------------------------------*
    "Tabela de InBound de CT-e Distribuida - Inf. Unid. Transp.
**----------------------------------------------------------------------*
    SELECT  * FROM zib_cte_dist_d55
      INTO TABLE @DATA(t_zib_cte_dist_d55)
      FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE cd_chave_cte = @t_zib_cte_dist_ter-cd_chave_cte.

    SORT t_zib_cte_dist_d55 BY cd_chave_cte valr_peso_rate DESCENDING.
**----------------------------------------------------------------------*
**Seleção para data de referência do Ct-e
**----------------------------------------------------------------------*

    SELECT * FROM zib_cte_dist_cpl
      INTO TABLE @DATA(t_zib_cte_dist_cpl)
      FOR ALL ENTRIES IN @t_zib_cte_dist_ter
         WHERE cd_chave_cte =  @t_zib_cte_dist_ter-cd_chave_cte.

**----------------------------------------------------------------------*
**Seleção para data de referência do Ct-e
**----------------------------------------------------------------------*

    SELECT * FROM zib_cte_dist_cvl
      INTO TABLE @DATA(t_zib_cte_dist_cvl)
      FOR ALL ENTRIES IN @t_zib_cte_dist_ter
         WHERE cd_chave_cte =  @t_zib_cte_dist_ter-cd_chave_cte.

**----------------------------------------------------------------------*
** Seleção nome filial
**----------------------------------------------------------------------*
    SELECT werks, name1 FROM t001w
      INTO TABLE @DATA(t_t001w)
         FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE werks = @t_zib_cte_dist_ter-f_tomadora.

**----------------------------------------------------------------------*
** Seleção nome fornecedor emissor
**----------------------------------------------------------------------*
    SELECT lifnr, name1, land1 FROM lfa1
      INTO TABLE @DATA(t_lfa1)
       FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE lifnr = @t_zib_cte_dist_ter-p_emissor
        AND land1 = 'BR'.

**----------------------------------------------------------------------*
** Tabela de InBound de Itens de Documento Fiscal
**----------------------------------------------------------------------*
    IF t_zib_cte_dist_n55[] IS NOT INITIAL.

      SELECT cd_chave_cte, docnum, itmnum, zmatnr_merc
         FROM zib_cte_dist_nit
        INTO TABLE @DATA(t_zib_cte_dist_nit)
         FOR ALL ENTRIES IN @t_zib_cte_dist_n55
        WHERE cd_chave_cte     = @t_zib_cte_dist_n55-cd_chave_cte
          AND docnum           = @t_zib_cte_dist_n55-docnum_nfe
          AND zmatnr_merc      IN @s_matnr.

    ELSE.

      MESSAGE 'Nenhum registro encontrado!'
      TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.

    ENDIF.

*----------------------------------------------------------------------*
** Seleção para material e grupo de mercadoria
**----------------------------------------------------------------------*
    IF t_zib_cte_dist_nit[] IS NOT INITIAL.

      "Textos breves de material
      SELECT matnr, matkl FROM mara
        INTO TABLE @DATA(t_mara)
        FOR ALL ENTRIES IN @t_zib_cte_dist_nit
        WHERE matnr =  @t_zib_cte_dist_nit-zmatnr_merc
         AND  matkl IN @s_matkl .

    ENDIF.

    IF t_mara[] IS NOT INITIAL.

      "Textos breves de material
      SELECT matnr, maktx FROM makt
        INTO TABLE @DATA(t_makt)
        FOR ALL ENTRIES IN @t_mara
        WHERE matnr =  @t_mara-matnr
          AND spras  = 'P'.

    ELSE.

      MESSAGE 'Nenhum registro encontrado!'
      TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.

    ENDIF.

    "Descritivo Grupo MateriaL
    IF t_mara[] IS NOT INITIAL.

      SELECT * FROM t023t
        INTO TABLE @DATA(t_t023t)
        FOR ALL ENTRIES IN @t_mara
        WHERE matkl = @t_mara-matkl.

    ENDIF.


**----------------------------------------------------------------------*
** Seleção Município e UF Expedidor
**----------------------------------------------------------------------*
    " ajuste de tamanho de campo
    MOVE-CORRESPONDING  t_zib_cte_dist_ter TO t_expedidor EXPANDING NESTED TABLES
                                              KEEPING TARGET LINES.

    SORT t_expedidor BY exped_tp_doc .
    DELETE t_expedidor WHERE exped_tp_doc  <> '1'
                          AND exped_tp_doc  <> '2' .

    IF t_expedidor[] IS NOT INITIAL.

      SELECT lifnr, land1, name1, stcd1, stcd2, txjcd, uf FROM lfa1
        INTO TABLE @DATA(t_lfa1_txjcd)
        FOR ALL ENTRIES IN @t_expedidor
        WHERE stcd1 =  @t_expedidor-exped_cnpj. "Pessoa Jurídica

      SELECT lifnr land1 name1 stcd1 stcd2 txjcd uf FROM lfa1
        APPENDING TABLE t_lfa1_txjcd
        FOR ALL ENTRIES IN t_expedidor
        WHERE stcd2 = t_expedidor-exped_cpf. "Pessoa Fisica

    ENDIF.

**----------------------------------------------------------------------*
** Seleção Município e UF  Recebedor
**----------------------------------------------------------------------*
    " ajuste de tamanho de campo
    MOVE-CORRESPONDING  t_zib_cte_dist_ter TO t_recebedor EXPANDING NESTED TABLES
                                              KEEPING TARGET LINES.

    SORT t_recebedor BY receb_tp_doc.
    DELETE t_recebedor WHERE exped_tp_doc   <> '1'
                          AND exped_tp_doc  <> '2' .

    IF t_recebedor[] IS NOT INITIAL.

* rjf - ini
      SELECT lifnr land1 name1 stcd1 stcd2 txjcd uf FROM lfa1
         APPENDING TABLE t_lfa1_txjcd
         FOR ALL ENTRIES IN t_recebedor
         WHERE stcd1 =  t_recebedor-receb_cnpj.

      SELECT lifnr land1 name1 stcd1 stcd2 txjcd uf FROM lfa1
        APPENDING TABLE t_lfa1_txjcd
        FOR ALL ENTRIES IN t_recebedor
        WHERE stcd2 =  t_recebedor-receb_cpf.
* rjf - fim

      SELECT lifnr, land1, name1, stcd1, stcd2, txjcd, uf FROM kna1
         INTO TABLE @DATA(t_kna1_txjcd)
        FOR ALL ENTRIES IN @t_recebedor
        WHERE stcd1 =  @t_recebedor-receb_cnpj.

      SELECT lifnr land1 name1 stcd1 stcd2 txjcd uf FROM kna1
        APPENDING TABLE t_kna1_txjcd
        FOR ALL ENTRIES IN t_recebedor
        WHERE stcd2 =  t_recebedor-receb_cpf.

    ENDIF.

    IF t_lfa1_txjcd[] IS NOT INITIAL.

      SELECT * FROM j_1btxjurt
        INTO TABLE @DATA(t_j_1btxjurt)
        FOR ALL ENTRIES IN @t_lfa1_txjcd
        WHERE country   = 'BR'
         AND taxjurcode =  @t_lfa1_txjcd-txjcd.

    ENDIF.

    IF t_kna1_txjcd[] IS NOT INITIAL.

      SELECT * FROM j_1btxjurt
        APPENDING TABLE t_j_1btxjurt
        FOR ALL ENTRIES IN t_kna1_txjcd
        WHERE country   = 'BR'
         AND taxjurcode =  t_kna1_txjcd-txjcd.

    ENDIF.

*----------------------------------------------------------------------*
** Seleção de dados para busca da tarifa frete
**----------------------------------------------------------------------*
    IF  t_zib_cte_dist_n55[] IS NOT INITIAL.

      DATA(t_n55_aux) = t_zib_cte_dist_n55.
      SORT t_n55_aux BY docnum_nfe.
      DELETE ADJACENT DUPLICATES FROM t_n55_aux COMPARING docnum_nfe.

      SELECT  docnum itmnum werks refkey reftyp FROM j_1bnflin
        INTO TABLE t_lin
        FOR ALL ENTRIES IN t_n55_aux
        WHERE docnum  = t_n55_aux-docnum_nfe
          AND itmnum  = '000010'.

      REFRESH: t_n55_aux.

    ENDIF.

    IF t_lin[] IS NOT INITIAL.

      SELECT  vbelv, posnv, vbeln, posnn, vbtyp_n FROM vbfa
        INTO TABLE @DATA(t_vbfa)
        FOR ALL ENTRIES IN @t_lin
        WHERE vbeln      =  @t_lin-refkey
          AND posnn      = '000010'
          AND vbtyp_n    = 'M'.

    ENDIF.

*----------------------------------------------------------------------
*Se J_1BNFLIN-REFTYP = “LI” , vamos  encontrar o de x para  na tabela ZLEST0041
*----------------------------------------------------------------------
    IF t_lin[] IS NOT INITIAL.

      SORT t_lin BY reftyp.
      DATA(t_reftyp_li) = t_lin.
      DELETE t_reftyp_li WHERE reftyp <> 'LI'.
* RJF - ini
*      DELETE t_lin       WHERE reftyp = 'LI'.
* RJF - Fim

      IF t_reftyp_li[] IS NOT INITIAL.

        SELECT docnum, branch, parid, nfenum, series FROM j_1bnfdoc
          INTO TABLE @DATA(t_doc)
          FOR ALL ENTRIES IN  @t_reftyp_li
          WHERE docnum = @t_reftyp_li-docnum.

      ENDIF.

      " Dados da nota de remessa por conta e ordem de terceiros
      IF t_doc[] IS NOT INITIAL.

        LOOP AT t_doc ASSIGNING FIELD-SYMBOL(<fs_doc>).
          UNPACK <fs_doc>-series TO <fs_doc>-series.
        ENDLOOP.

        SELECT centro_comprador, nr_nf, serie, nr_nf_propria, cod_cliente, docnum FROM zlest0041
          INTO TABLE @DATA(t_zlest0041)
          FOR ALL ENTRIES IN @t_doc
          WHERE centro_comprador = @t_doc-branch
            AND cod_cliente      = @t_doc-parid
            AND nr_nf            = @t_doc-nfenum
            AND serie            = @t_doc-series.

      ENDIF.

      IF t_zlest0041[] IS NOT INITIAL.

        SELECT  docnum itmnum werks refkey reftyp FROM j_1bnflin
          INTO TABLE t_lin_li
          FOR ALL ENTRIES IN t_zlest0041
          WHERE docnum  = t_zlest0041-docnum
            AND itmnum  = '000010'
            AND reftyp = 'BI'.

      ENDIF.

    ENDIF.

    IF t_lin_li[] IS NOT INITIAL.

      SELECT  vbelv posnv vbeln posnn vbtyp_n FROM vbfa
        APPENDING TABLE t_vbfa
        FOR ALL ENTRIES IN t_lin_li
        WHERE vbeln      =  t_lin_li-refkey
          AND posnn      = '000010'
          AND vbtyp_n    = 'M'.

    ENDIF.

    DATA: r_parvw TYPE RANGE OF parvw.

    IF t_vbfa[] IS NOT INITIAL.

      r_parvw = VALUE #( ( sign = 'I' option = 'EQ' low = 'PC' )
                         ( sign = 'I' option = 'EQ' low = 'LR' )
                         ( sign = 'I' option = 'EQ' low = 'Z1' ) ).

      SELECT vbeln, posnr, parvw, lifnr, adrnr,  lzone FROM vbpa
      INTO TABLE @DATA(t_vbpa)
        FOR ALL ENTRIES IN @t_vbfa
      WHERE vbeln  = @t_vbfa-vbeln
        AND parvw IN @r_parvw.

    ENDIF.

    IF t_vbpa[] IS NOT INITIAL.

      SORT t_vbpa BY parvw.
      DATA(t_vbpa_pc) = t_vbpa.
      DELETE t_vbpa_pc WHERE parvw <> 'PC'.

      "Endereços (administração de endereços central)
      SELECT addrnumber, date_from, nation, taxjurcode, city1 FROM adrc
        INTO TABLE @DATA(t_adrc)
        FOR ALL ENTRIES IN @t_vbpa_pc
        WHERE addrnumber = @t_vbpa_pc-adrnr.

    ENDIF.

  ELSE.

    MESSAGE 'Nenhum registro encontrado!'
    TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

**&---------------------------------------------------------------------*
**&    Montar Saída
**&---------------------------------------------------------------------*
  DATA: w_diverg LIKE LINE OF t_saida_diverg.

  DATA: wa_cellcolor TYPE lvc_s_scol.
  DATA: ld_index TYPE sy-tabix.

  LOOP AT t_zib_cte_dist_ter INTO DATA(w_zib_cte_dist_ter).

    CLEAR: w_diverg.

    ld_index = sy-tabix.
    MOVE-CORRESPONDING w_zib_cte_dist_ter TO w_diverg.

    "Seleção para data de referência do Ct-e
    READ TABLE t_zib_cte_dist_cpl INTO DATA(w_zib_cte_dist_cpl)
                               WITH KEY cd_chave_cte =  w_zib_cte_dist_ter-cd_chave_cte
                                        ds_campo = 'DATA:'.

    IF sy-subrc IS INITIAL.
      REPLACE ALL OCCURRENCES OF '/' IN w_zib_cte_dist_cpl-ds_texto WITH '.'.
      SPLIT w_zib_cte_dist_cpl-ds_texto AT '.' INTO DATA(l_dia) DATA(l_mes) DATA(l_ano).
      CONCATENATE l_ano l_mes l_dia INTO w_diverg-data_ref.
    ENDIF.

    "seleção para tarifa cobrada
    READ TABLE t_zib_cte_dist_cvl INTO DATA(w_zib_cte_dist_cvl)
                               WITH KEY cd_chave_cte =  w_zib_cte_dist_ter-cd_chave_cte
                                        nome_componente = 'Tarifa' .

    IF sy-subrc NE 0."Caso no XML estivesse TARIFA
      READ TABLE t_zib_cte_dist_cvl INTO w_zib_cte_dist_cvl
                             WITH KEY cd_chave_cte =  w_zib_cte_dist_ter-cd_chave_cte
                                      nome_componente = 'TARIFA' .
    ENDIF.

    IF sy-subrc IS INITIAL.
      w_diverg-tarifa_cobrada = w_zib_cte_dist_cvl-valr_componente.
    ENDIF.

    "Nome filial CTe
    READ TABLE t_t001w INTO DATA(w_t001w)
                                   WITH KEY werks = w_zib_cte_dist_ter-f_tomadora.
    IF sy-subrc IS INITIAL.
      w_diverg-f_name1 =  w_t001w-name1.
    ENDIF.

    "Nome forn. emissor
    READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = w_zib_cte_dist_ter-p_emissor.
    IF sy-subrc IS INITIAL.
      w_diverg-p_emi_name1 =  w_lfa1-name1.
    ENDIF.

    READ TABLE t_zib_cte_dist_d55 INTO DATA(w_zib_cte_dist_d55)
                                  WITH KEY cd_chave_cte  = w_zib_cte_dist_ter-cd_chave_cte.

    IF sy-subrc IS INITIAL.

      READ TABLE t_zib_cte_dist_n55 INTO DATA(w_zib_cte_dist_n55)
                                    WITH KEY cd_chave_cte     = w_zib_cte_dist_d55-cd_chave_cte
                                             n55_chave_acesso = w_zib_cte_dist_d55-n55_chave_acesso.

      IF sy-subrc IS INITIAL.

        "Material
        READ TABLE t_zib_cte_dist_nit INTO DATA(w_zib_cte_dist_nit)
                                       WITH KEY cd_chave_cte     = w_zib_cte_dist_n55-cd_chave_cte
                                                docnum           = w_zib_cte_dist_n55-docnum_nfe.
        CHECK sy-subrc IS INITIAL.
        w_diverg-zmatnr_merc = w_zib_cte_dist_nit-zmatnr_merc.
      ENDIF.

      "Gp. Material
      READ TABLE t_mara INTO DATA(w_mara) WITH KEY matnr = w_diverg-zmatnr_merc.

*rjf
*      CHECK sy-subrc IS INITIAL.
      IF sy-subrc IS INITIAL.
        w_diverg-matkl        = w_mara-matkl.
        READ TABLE t_t023t INTO DATA(w_t023t) WITH KEY matkl = w_mara-matkl.
        IF sy-subrc EQ 0.
          w_diverg-wgbez60 = w_t023t-wgbez60.
        ENDIF.
      ELSE.
        w_diverg-status = '@09@'."ICON
      ENDIF.

      "Descr. Material
      READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_diverg-zmatnr_merc.
      IF sy-subrc IS INITIAL.
        w_diverg-maktx = w_makt-maktx.
      ENDIF.

    ENDIF.

    "Município de coleta
    CONCATENATE w_zib_cte_dist_ter-inicio_muni w_zib_cte_dist_ter-inicio_uf
                  INTO w_diverg-muni_coleta SEPARATED BY space.

    "Município de entrega
    CONCATENATE w_zib_cte_dist_ter-termino_muni w_zib_cte_dist_ter-termino_uf
      INTO w_diverg-muni_entrega SEPARATED BY space.

    "CNPJ/CPF Expedidor - Pesso Jurídica
    IF w_zib_cte_dist_ter-exped_tp_doc = '1'.
      w_diverg-expedidor = w_zib_cte_dist_ter-exped_cnpj.
      WRITE w_diverg-expedidor TO w_diverg-expedidor USING EDIT MASK '__.___.___/____-__'.

      "Município/UF expedidor
      READ TABLE t_lfa1_txjcd INTO DATA(w_lfa1_txjcd)
                              WITH KEY stcd1 = w_zib_cte_dist_ter-exped_cnpj.
      IF sy-subrc IS INITIAL.
        w_diverg-cod_exp =  w_lfa1_txjcd-lifnr.
*          W_DIVERG-EXPED_RSOCIAL =  W_LFA1_TXJCD-NAME1.
        READ TABLE t_j_1btxjurt INTO DATA(w_j_1btxjurt) WITH KEY taxjurcode = w_lfa1_txjcd-txjcd.
        IF sy-subrc IS INITIAL.
          CONCATENATE w_j_1btxjurt-text w_lfa1_txjcd-txjcd(2) INTO w_diverg-muni_exped SEPARATED BY space.
        ENDIF.
      ENDIF.

    ENDIF.

    "CNPJ/CPF Expedidor - Pesso Fisica
    IF w_zib_cte_dist_ter-exped_tp_doc = '2'.
      w_diverg-expedidor = w_zib_cte_dist_ter-exped_cpf.
      WRITE w_diverg-expedidor TO w_diverg-expedidor USING EDIT MASK '___.___.___-__'.

      READ TABLE t_lfa1_txjcd INTO  w_lfa1_txjcd
                              WITH KEY stcd2 = w_zib_cte_dist_ter-exped_cpf.
      IF sy-subrc IS INITIAL.
        w_diverg-cod_exp =  w_lfa1_txjcd-lifnr.
*          W_DIVERG-EXPED_RSOCIAL =  W_LFA1_TXJCD-NAME1.

        READ TABLE t_j_1btxjurt INTO w_j_1btxjurt WITH KEY taxjurcode = w_lfa1_txjcd-txjcd.
        IF sy-subrc IS INITIAL.
          CONCATENATE w_j_1btxjurt-text w_lfa1_txjcd-txjcd(2) INTO w_diverg-muni_exped SEPARATED BY space.
        ENDIF.
      ENDIF.

    ENDIF.

    "CNPJ/CPF Recebedor - Pesso Jurídica
    IF w_zib_cte_dist_ter-exped_tp_doc = '1'.
      w_diverg-recebedor  = w_zib_cte_dist_ter-receb_cnpj.

      WRITE w_diverg-recebedor TO w_diverg-recebedor USING EDIT MASK '__.___.___/____-__'.

      "Município/UF expedidor
      READ TABLE t_kna1_txjcd INTO DATA(w_kna1_txjcd)
                              WITH KEY stcd1 = w_zib_cte_dist_ter-receb_cnpj.
      IF sy-subrc IS INITIAL AND w_kna1_txjcd-lifnr IS NOT INITIAL.
        w_diverg-cod_rec =  w_kna1_txjcd-lifnr.
*          W_DIVERG-RECEB_RSOCIAL =  W_KNA1_TXJCD-NAME1.

        READ TABLE t_j_1btxjurt INTO w_j_1btxjurt WITH KEY taxjurcode =  w_kna1_txjcd-txjcd.

        IF sy-subrc IS INITIAL.
          CONCATENATE w_j_1btxjurt-text w_kna1_txjcd-txjcd(2) INTO w_diverg-muni_receb SEPARATED BY space.
        ENDIF.
* rjf - ini
      ELSE.

        READ TABLE t_lfa1_txjcd INTO w_kna1_txjcd
                                WITH KEY stcd1 = w_zib_cte_dist_ter-receb_cnpj.
        IF sy-subrc IS INITIAL.
          w_diverg-cod_rec =  w_kna1_txjcd-lifnr.
        ENDIF.
* rjf - fim
      ENDIF.

    ENDIF.

    "CNPJ/CPF Recebedor- Pesso Fisica
    IF w_zib_cte_dist_ter-exped_tp_doc = '2'.

      w_diverg-recebedor = w_zib_cte_dist_ter-receb_cpf.
      WRITE w_diverg-recebedor TO w_diverg-recebedor USING EDIT MASK '___.___.___-__'.

      READ TABLE t_kna1_txjcd INTO w_kna1_txjcd
                              WITH KEY stcd2 = w_zib_cte_dist_ter-receb_cpf.
      IF sy-subrc IS INITIAL AND w_kna1_txjcd IS NOT INITIAL.
        w_diverg-cod_rec =  w_kna1_txjcd-lifnr.
*          W_DIVERG-RECEB_RSOCIAL =  W_KNA1_TXJCD-NAME1.

        READ TABLE t_j_1btxjurt INTO w_j_1btxjurt WITH KEY taxjurcode =  w_kna1_txjcd-txjcd.
        IF sy-subrc IS INITIAL.
          CONCATENATE w_j_1btxjurt-text w_kna1_txjcd-txjcd(2) INTO w_diverg-muni_receb SEPARATED BY space.
        ENDIF.

* rjf - ini
      ELSE.
        READ TABLE t_lfa1_txjcd INTO w_kna1_txjcd
                                WITH KEY stcd2 = w_zib_cte_dist_ter-receb_cpf.
        IF sy-subrc IS INITIAL.
          w_diverg-cod_rec =  w_kna1_txjcd-lifnr.
        ENDIF.
* rjf - fim
      ENDIF.
    ENDIF.

*----------------------------------------------------------------------
* Buscar tarifa frete
*----------------------------------------------------------------------
    "Busca Local de expedição
    READ TABLE t_lin INTO DATA(w_lin) WITH KEY docnum  = w_zib_cte_dist_n55-docnum_nfe
                                               itmnum  = '000010'
                                               reftyp  = 'BI'.
    IF sy-subrc IS INITIAL.

      w_diverg-local_exped = w_lin-werks.

    ELSE.

      "DE X PARA
      READ TABLE t_reftyp_li INTO DATA(w_reftyp_li) WITH KEY docnum  = w_zib_cte_dist_n55-docnum_nfe
                                                 itmnum  = '000010'
                                                 reftyp  = 'LI'.
      IF sy-subrc IS INITIAL.

        READ TABLE t_doc INTO DATA(w_doc) WITH KEY docnum = w_reftyp_li-docnum.

        IF sy-subrc IS INITIAL.

          READ TABLE t_zlest0041 INTO DATA(w_zlest0041) WITH KEY centro_comprador = w_doc-branch
                                                                 cod_cliente      = w_doc-parid
                                                                 nr_nf            = w_doc-nfenum
                                                                 serie            = w_doc-series.
          IF sy-subrc IS INITIAL.

            READ TABLE t_lin_li INTO w_lin WITH KEY docnum  = w_zlest0041-docnum
                                                             itmnum  = '000010'
                                                               reftyp = 'BI'.
            IF sy-subrc IS INITIAL.
              w_diverg-local_exped = w_lin-werks.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF  w_lin-refkey IS NOT INITIAL.

      "Buscar LZONE
      READ TABLE t_vbfa INTO DATA(w_vbfa) WITH KEY vbeln    = w_lin-refkey
                                                   posnn    = '000010'
                                                   vbtyp_n  = 'M'.
      IF sy-subrc IS INITIAL.

        "Busca Ponto de coleta
        READ TABLE t_vbpa INTO DATA(w_vbpa_pc) WITH KEY vbeln = w_vbfa-vbeln
                                                        parvw = 'PC'.
        IF sy-subrc IS INITIAL.
          w_diverg-ponto_coleta = w_vbpa_pc-lifnr.

          READ TABLE t_adrc INTO DATA(w_adrc) WITH KEY addrnumber = w_vbpa_pc-adrnr.
          IF sy-subrc IS  INITIAL.
            w_diverg-city1       = w_adrc-city1.      "  ( cidade do parceiro PC)
            w_diverg-taxjurcode = w_adrc-taxjurcode. " ( Domicilio Fiscal do Parceiro PC
          ENDIF.

        ENDIF.

        "Busca LZONE_LR
        READ TABLE t_vbpa INTO DATA(w_vbpa_lr) WITH KEY vbeln = w_vbfa-vbeln
                                                        parvw = 'LR'.
        IF sy-subrc IS INITIAL.
          w_diverg-lzone_lr = w_vbpa_lr-lzone.
        ENDIF.

        "Busca LZONE_Z1
        READ TABLE t_vbpa INTO DATA(w_vbpa_z1) WITH KEY vbeln = w_vbfa-vbeln
                                                        parvw =  'Z1'.
        IF sy-subrc IS INITIAL.
          w_diverg-lzone_z1 = w_vbpa_z1-lzone.
        ENDIF.

      ENDIF.

      IF w_vbpa_lr-lzone = w_vbpa_z1-lzone.
        w_diverg-shtyp =  'Z028'.
      ELSE.
        w_diverg-shtyp =  'Z003'.
      ENDIF.

      "Tarifa Sap
      PERFORM zf_buscar_tarifa USING w_diverg-p_emissor
                                     w_diverg-ponto_coleta
                                     w_diverg-shtyp
                                     w_diverg-lzone_lr
                                     w_diverg-lzone_z1
                                     w_diverg-data_ref
                                     w_diverg-local_exped
                                     w_diverg-zmatnr_merc
                                     CHANGING w_diverg-kbetr
                                              w_diverg-tipo_tk
                                              w_diverg-cod_postal.

    ENDIF.

    IF w_diverg-status = '@09@'.
      w_diverg-status = '@09@'.
    ELSEIF w_diverg-tarifa_cobrada <> w_diverg-kbetr.
      w_diverg-status = '@0A@'."ICON_RED_LIGHT
    ELSE.
      w_diverg-status = '@08@'."ICON_GREEN_LIGHT
    ENDIF.

    APPEND w_diverg TO t_saida_diverg.
    CLEAR: w_diverg, w_zib_cte_dist_cpl, w_zib_cte_dist_d55, w_zlest0041, w_lin, "w_lin_li,
           w_t001w, w_lfa1.

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

*  READ TABLE t_saida INTO DATA(w_saida) INDEX p_index.

  CASE p_column.
    WHEN 'TKNUM'.
*      PERFORM zf_call_transaction_vt03n USING w_saida.
    WHEN 'FKNUM'.
*      PERFORM zf_call_transaction_vi03 USING w_saida.
    WHEN 'VBELN_VL'.
*      PERFORM zf_call_transaction_vl03n USING w_saida.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_VI03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SAIDA  text
*----------------------------------------------------------------------*
*FORM zf_call_transaction_vi03   USING    p_saida TYPE y_saida.
*  SET PARAMETER ID 'FKK' FIELD p_saida-fknum.
*  CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_VT03N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SAIDA  text
*----------------------------------------------------------------------*
*FORM zf_call_transaction_vt03n   USING    p_saida TYPE y_saida.
*  SET PARAMETER ID 'TNR' FIELD p_saida-tknum.
*  CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_VL03N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SAIDA  text
*----------------------------------------------------------------------*
*FORM zf_call_transaction_vl03n  USING  p_saida TYPE y_saida.
*  SET PARAMETER ID 'VL' FIELD p_saida-vbeln_vl.
*  CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_status_9000 OUTPUT.
  SET PF-STATUS 'ZGMM_STATUS'.
  PERFORM zf_preparar_alv.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_alv.

  DATA: t_fcat TYPE lvc_t_fcat.

  DATA: w_variant TYPE disvariant,
        w_layout  TYPE lvc_s_layo.

  w_layout-cwidth_opt = 'X'.
  w_layout-zebra      = 'X'.
  w_layout-sel_mode   = 'A'.

  w_layout-info_fname = 'COLOR_LINE'.
  w_layout-ctab_fname = 'COLOR_CELL'.

  w_variant-report   = sy-repid.
  w_variant-variant  = p_layout.

  "Criar tela padrão Amaggi
  IF v_grid IS INITIAL.

    "Preencher Textos do cabeçalho
    PERFORM zf_textos_header.
    PERFORM zf_split_screen        CHANGING v_grid.

    CREATE OBJECT v_event_receiver.
    SET HANDLER v_event_receiver->handle_hotspot_click FOR v_grid.
    SET HANDLER v_event_receiver->handle_toolbar       FOR v_grid.
    SET HANDLER v_event_receiver->handle_user_command  FOR v_grid.

    IF r_div IS NOT INITIAL.

      PERFORM zf_montar_fieldcat     CHANGING t_saida_diverg t_fcat.
      PERFORM zf_ajuste_descr_campos_diverg CHANGING t_fcat.

      LOOP AT t_saida_diverg ASSIGNING FIELD-SYMBOL(<fs_saida_diverg>).
        <fs_saida_diverg>-color_cell = t_coltab.
      ENDLOOP.

      CALL METHOD v_grid->set_table_for_first_display
        EXPORTING
          is_variant                    = w_variant
          i_save                        = 'A'
          is_layout                     = w_layout
        CHANGING
          it_fieldcatalog               = t_fcat
          it_outtab                     = t_saida_diverg
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

    IF r_rat IS NOT INITIAL.

*      PERFORM zf_split_screen        CHANGING v_grid.
      PERFORM zf_montar_fieldcat     CHANGING t_saida_rateio t_fcat.
      PERFORM zf_ajuste_descr_campos_rateio CHANGING t_fcat.

      CALL METHOD v_grid->set_table_for_first_display
        EXPORTING
          is_variant                    = w_variant
          i_save                        = 'A'
          is_layout                     = w_layout
        CHANGING
          it_fieldcatalog               = t_fcat
          it_outtab                     = t_saida_rateio
*         IT_SORT                       = T_SORT
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

*   RAISE EVENT TOOLBAR TO SHOW THE MODIFIED TOOLBAR
*    CALL METHOD v_grid2->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SPLIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_split_screen  CHANGING p_alv TYPE REF TO cl_gui_alv_grid.

  DATA: l_header       TYPE REF TO cl_gui_splitter_container,
        l_picture      TYPE REF TO cl_gui_picture,
        l_dg_dyndoc_id TYPE REF TO cl_dd_document,
        l_html         TYPE REF TO  cl_gui_html_viewer,
        l_split        TYPE REF TO  cl_gui_splitter_container.

  DATA: t_text_table_f  TYPE sdydo_text_table,
        t_text_table_v  TYPE sdydo_text_table,
        t_text_table_f2 TYPE sdydo_text_table,
        t_text_table_v2 TYPE sdydo_text_table.

  CHECK l_split IS INITIAL.

  l_split = NEW #( parent = cl_gui_container=>screen0 rows = 2 columns = 1 ).

  DATA(v_container_html) = l_split->get_container( EXPORTING row = 1 column = 1 ).

*----------------------------------------------------------------------*
* Header
*----------------------------------------------------------------------*
  l_header = NEW #( parent = v_container_html rows = 1 columns = 2 ).

  DATA(l_header_texto) = l_header->get_container( EXPORTING row = 1 column = 1 ).

  l_header->set_column_width( EXPORTING id = 1 width = 40 ).

  "Logo
  DATA(l_header_logo) = l_header->get_container( EXPORTING row = 1 column = 2 ).

  l_picture = NEW #( parent = l_header_logo ).

  DATA: l_url TYPE char255.

  PERFORM zf_buscar_imagem_url USING 'LOGO_NOVO' CHANGING l_url.

  l_picture->load_picture_from_url( EXPORTING url = l_url ).

  l_picture->set_display_mode( EXPORTING display_mode = l_picture->display_mode_fit_center ).

*----------------------------------------------------------------------*
* Item
*----------------------------------------------------------------------*

  DATA(v_item_grid) = l_split->get_container( EXPORTING row = 2 column = 1 ).

  l_split->set_row_height( EXPORTING id = 1 height = 15 ).

  p_alv = NEW #( i_parent = v_item_grid ).

  l_dg_dyndoc_id = NEW #( style = 'ALV_TO_HTML' background_color = 7 ).
  l_dg_dyndoc_id->initialize_document( ).

  l_dg_dyndoc_id->add_table( EXPORTING no_of_columns = 1 border = '0' width = '100%' IMPORTING table = DATA(table_element) ).

*----------------------------------------------------------------------*
* Preencher Titulo
*----------------------------------------------------------------------*
  IF v_titulo IS NOT INITIAL.
    table_element->add_column( IMPORTING column = DATA(column) ).
    table_element->set_column_style( EXPORTING col_no = 1 sap_style = cl_dd_document=>heading sap_align = 'CENTER' ).
    column->add_text( EXPORTING text = CONV #( v_titulo ) sap_style = 'HEADING' ).
  ENDIF.

*----------------------------------------------------------------------*
* Mostra dados adicionais
*----------------------------------------------------------------------*
  IF t_filtro[] IS NOT INITIAL.

    l_dg_dyndoc_id->add_table( EXPORTING no_of_columns = 4 border = '0' width = '100%' IMPORTING table = DATA(table_element_linhas) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_1) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_2) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_3) ).
    table_element_linhas->add_column( IMPORTING column = DATA(column_4) ).

    table_element_linhas->set_column_style( EXPORTING col_no = 1 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
    table_element_linhas->set_column_style( EXPORTING col_no = 2 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).
    table_element_linhas->set_column_style( EXPORTING col_no = 3 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
    table_element_linhas->set_column_style( EXPORTING col_no = 4 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).

    LOOP AT t_filtro INTO DATA(w_filtro).

      APPEND w_filtro-parametro TO t_text_table_f.
      APPEND w_filtro-valor TO t_text_table_v.

      APPEND w_filtro-parametro2 TO t_text_table_f2.
      APPEND w_filtro-valor2 TO t_text_table_v2.

      column_1->add_text( EXPORTING text_table = t_text_table_f  fix_lines = abap_true ).
      column_2->add_text( EXPORTING text_table = t_text_table_v  fix_lines = abap_true ).
      column_3->add_text( EXPORTING text_table = t_text_table_f2 fix_lines = abap_true ).
      column_4->add_text( EXPORTING text_table = t_text_table_v2 fix_lines = abap_true ).

      CLEAR: t_text_table_f[], t_text_table_v[], t_text_table_f2[], t_text_table_v2[].

    ENDLOOP.

  ENDIF.

  l_html = NEW #( parent = l_header_texto ).

  l_dg_dyndoc_id->merge_document( ).

  l_dg_dyndoc_id->html_control = l_html.

  l_dg_dyndoc_id->display_document( EXPORTING reuse_control = 'X' parent = l_header_texto ).


  CALL METHOD l_split->set_row_height
    EXPORTING
      id     = 1
      height = 20.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat  CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
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

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_IMAGEM_URL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0922   text
*      <--P_L_URL  text
*----------------------------------------------------------------------*
FORM zf_buscar_imagem_url   USING    i_nome_logo
                         CHANGING r_url.


  TYPES: BEGIN OF ty_graphic_table,
           line(255) TYPE x,
         END OF ty_graphic_table.

  DATA: graphic_table TYPE TABLE OF ty_graphic_table.

  DATA: l_graphic_xstr TYPE xstring.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = i_nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  DATA(graphic_size) = xstrlen( l_graphic_xstr ).
  DATA(l_graphic_conv) = graphic_size.
  DATA(l_graphic_offs) = 0.
  WHILE l_graphic_conv > 255.
    APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(255) ) TO graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(l_graphic_conv) ) TO graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = r_url.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_9000 INPUT.
*  CASE sy-ucomm.
*    WHEN 'BACK'.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_TEXTOS_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_textos_header .

  REFRESH: t_filtro. CLEAR: v_titulo.

  "Texto titulo do Cabeçalho
  IF r_div IS NOT INITIAL.
    v_titulo            = 'Divergência de Tarifa'.
  ENDIF.

  IF r_rat IS NOT INITIAL.
    v_titulo            = 'Rateio NF vinculadas ao CT-e'.
  ENDIF.

  " textos
  LOOP AT SCREEN.
    t_filtro = VALUE #(
      ( parametro = ' ' valor = '' )
    ).
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_monta_tela .

  LOOP AT SCREEN.

    IF r_div IS NOT INITIAL.

      IF screen-group1 EQ 'RAT'.
        screen-invisible = 1.
        screen-input     = 0.
      ENDIF.

    ENDIF.

    IF r_rat IS NOT INITIAL.

      IF screen-group1 EQ 'DIV'.
        screen-invisible = 1.
        screen-input     = 0.
      ENDIF.

    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDAR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_validar_campos.

**----------------------------------------------------------------------*
* Preencher zeros esquerda
**----------------------------------------------------------------------*
  LOOP AT s_numcte ASSIGNING FIELD-SYMBOL(<fs_numcte>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_numcte>-low
      IMPORTING
        output = <fs_numcte>-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_numcte>-high
      IMPORTING
        output = <fs_numcte>-high.

  ENDLOOP.

  LOOP AT s_numnfe ASSIGNING FIELD-SYMBOL(<fs_numnfe>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_numnfe>-low
      IMPORTING
        output = <fs_numnfe>-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_numnfe>-high
      IMPORTING
        output = <fs_numnfe>-high.

  ENDLOOP.

  LOOP AT s_numnft ASSIGNING FIELD-SYMBOL(<fs_numnft>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_numnft>-low
      IMPORTING
        output = <fs_numnft>-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_numnft>-high
      IMPORTING
        output = <fs_numnft>-high.

  ENDLOOP.

**----------------------------------------------------------------------*
* Campo empresa obrigatório
**----------------------------------------------------------------------*
  IF s_bukrs[] IS INITIAL.
    MESSAGE 'Informe o código da Empresa tomadora!'
          TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

**----------------------------------------------------------------------*
*se inform. data de emissão, não precisa dos demais campos de seleção.
**----------------------------------------------------------------------*
  IF s_budat[] IS INITIAL AND

**----------------------------------------------------------------------*
*se usuário inform chave de ct-e, não precisa inf. demais campos de seleção.
**----------------------------------------------------------------------*
   s_chcte[] IS INITIAL AND

*----------------------------------------------------------------------*
*    se usuário inform. chave de nf-e  não precisa inf demais campos
*----------------------------------------------------------------------*
   s_chnfe[] IS INITIAL.


    IF s_lifnr[] IS INITIAL AND s_stcd1[] IS INITIAL AND
         s_numcte[] IS INITIAL AND s_numnfe[] IS INITIAL
         AND s_numnft[] IS INITIAL.
      MESSAGE 'Parâmetros de seleção insuficientes. Informe a Data Emissão CT-e'
            TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
**----------------------------------------------------------------------*
* se inform. fornecedor é preciso que ele informe também data de emissão.
**----------------------------------------------------------------------*
    IF s_lifnr[] IS NOT INITIAL AND s_budat IS INITIAL.
      MESSAGE 'Informe a Data Emissão CT-e!'
            TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

**----------------------------------------------------------------------*
*    se usuário informar cnpj fornecedor é preciso que ele informe também data de emissão.
**----------------------------------------------------------------------*
    IF s_stcd1[] IS NOT INITIAL AND s_budat IS INITIAL.
      MESSAGE 'Informe a Data Emissão CT-e!'
            TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

**----------------------------------------------------------------------*
*    se usuário informar nr. de  ct-e  é preciso também informar data de emissão.
**----------------------------------------------------------------------*
    IF s_numcte[] IS NOT INITIAL AND s_budat IS INITIAL.
      MESSAGE 'Informe a Data Emissão CT-e!'
            TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

**----------------------------------------------------------------------*
*    se usuário informar nr. de nf-e é preciso também informar data de emissão.
**----------------------------------------------------------------------*
    IF s_numnfe[] IS NOT INITIAL AND s_budat IS INITIAL.
      MESSAGE 'Informe a Data Emissão CT-e!'
            TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

**----------------------------------------------------------------------*
*    se usuário informar nr. de nf-e de terceiro é preciso também informar data de emissão.
**----------------------------------------------------------------------*
    IF s_numnft[] IS NOT INITIAL AND s_budat IS INITIAL.
      MESSAGE 'Informe a Data Emissão CT-e!'
            TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.

*  ENDIF.

  "Valida acesso a empresa tomadora
  SELECT * FROM t001
    INTO TABLE @DATA(t_t001)
    WHERE bukrs IN @s_bukrs.

  IF t_t001[] IS NOT INITIAL.

    LOOP AT t_t001 INTO DATA(w_t001).

      AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
               ID 'ACTVT' FIELD '03'
               ID 'BUKRS' FIELD w_t001-bukrs.
      IF sy-subrc <> 0.
        DELETE t_t001 INDEX sy-tabix.

      ELSE.

        r_bukrs = VALUE #( ( sign = 'I' option = 'EQ' low = w_t001-bukrs ) ).

      ENDIF.

    ENDLOOP.

  ENDIF.

  IF t_t001[] IS INITIAL.
    MESSAGE 'Sem autorização na Empresa tomadora!'
    TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF r_div IS NOT INITIAL.
    PERFORM zf_preparar_dados_divergencia.
  ENDIF.

  IF r_rat IS NOT INITIAL.
    PERFORM zf_preparar_dados_rateio.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_DADOS_RATEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_dados_rateio.

  DATA: t_n55_ch       TYPE TABLE OF y_n55_ch.

  DATA: w_chave_acesso TYPE j_1b_nfe_access_key,
        w_n55_ch       LIKE LINE OF t_n55_ch,
        w_rateio       LIKE LINE OF t_saida_rateio.

  DATA: r_cd_modal TYPE RANGE OF zmodal.

  r_cd_modal = VALUE #( ( sign = 'I' option = 'EQ' low = '04' )
                         ( sign = 'I' option = 'EQ' low = '06' ) ).

  IF s_chcte[] IS INITIAL AND s_chnfe[] IS NOT INITIAL.

*   "Tabela de InBound de CT-e Distribuida - NF55
    SELECT cd_chave_cte, n55_chave_acesso, docnum_nfe, branch, parvw, parid,
            tknum, fknum, vbeln_vl FROM zib_cte_dist_n55
    INTO TABLE @DATA(t_zib_cte_dist_n55)
    WHERE n55_chave_acesso IN @s_chnfe
      AND bukrs  IN @r_bukrs
      AND branch IN @s_branch.

    IF t_zib_cte_dist_n55[] IS NOT INITIAL.

      SELECT cd_chave_cte, numr_cte, numr_serie, dt_emissao, valor_prestacao,
        cd_tomador, emit_cnpj, emit_rsocial, exped_tp_doc, exped_cnpj, receb_cnpj, dest_cnpj,
        inicio_muni, termino_muni, ds_prod_pred, qt_carga_cte, e_tomadora,
        f_tomadora, p_emissor, belnr, peso_chegada, dt_chegada
          FROM zib_cte_dist_ter
          INTO TABLE @DATA(t_zib_cte_dist_ter)
        FOR ALL ENTRIES IN @t_zib_cte_dist_n55
          WHERE cd_chave_cte   = @t_zib_cte_dist_n55-cd_chave_cte
            AND numr_cte       IN @s_numcte
                AND dt_emissao IN @s_budat
                AND emit_cnpj  IN @s_stcd1
                "AND cd_modal = '04'
                 AND cd_modal IN @r_cd_modal
                AND e_tomadora IN @r_bukrs
                AND f_tomadora IN @s_branch
                AND p_emissor  IN @s_lifnr.

    ENDIF.

    REFRESH: t_zib_cte_dist_n55.

  ELSE.

**----------------------------------------------------------------------*
**Seleção para data de referência do Ct-e
**----------------------------------------------------------------------*

    "Tabela de InBound de CT-e Distribuida
    SELECT cd_chave_cte numr_cte numr_serie dt_emissao valor_prestacao
         cd_tomador emit_cnpj emit_rsocial exped_tp_doc exped_cnpj receb_cnpj dest_cnpj
         inicio_muni termino_muni ds_prod_pred qt_carga_cte e_tomadora
         f_tomadora p_emissor belnr peso_chegada dt_chegada
          FROM zib_cte_dist_ter
          INTO TABLE t_zib_cte_dist_ter
          WHERE cd_chave_cte   IN s_chcte
            AND numr_cte       IN s_numcte
                AND dt_emissao IN s_budat
                AND emit_cnpj  IN s_stcd1
               " AND cd_modal = '04'
                AND cd_modal IN r_cd_modal
                AND e_tomadora IN r_bukrs
                AND f_tomadora IN s_branch
                AND p_emissor  IN s_lifnr.

  ENDIF.

  IF t_zib_cte_dist_ter[] IS NOT INITIAL.

**----------------------------------------------------------------------*
** Seleção nome filial
**----------------------------------------------------------------------*
    SELECT werks, name1 FROM t001w
      INTO TABLE @DATA(t_t001w)
         FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE werks = @t_zib_cte_dist_ter-f_tomadora.

**----------------------------------------------------------------------*
** Seleção nome fornecedor emissor
**----------------------------------------------------------------------*
    SELECT lifnr, name1, land1 FROM lfa1
      INTO TABLE @DATA(t_lfa1)
       FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE lifnr = @t_zib_cte_dist_ter-p_emissor
        AND land1 = 'BR'.

**----------------------------------------------------------------------*
    "Tabela de InBound de CT-e Distribuida - NF55
**----------------------------------------------------------------------*
    SELECT cd_chave_cte n55_chave_acesso docnum_nfe branch parvw parid tknum fknum vbeln_vl FROM zib_cte_dist_n55
      INTO TABLE t_zib_cte_dist_n55
      FOR ALL ENTRIES IN t_zib_cte_dist_ter
      WHERE cd_chave_cte = t_zib_cte_dist_ter-cd_chave_cte.
*        AND bukrs  IN r_bukrs      "Comentado. / CS2021000837 - ZMM0172 - Consulta de documentos (Notas São Francisco do Sul) / Anderson Oenning
*        AND branch IN s_branch.    "Comentado. / CS2021000837 - ZMM0172 - Consulta de documentos (Notas São Francisco do Sul) / Anderson Oenning

**----------------------------------------------------------------------*
    "Tabela de InBound de CT-e Distribuida - Inf. Unid. Transp.
**----------------------------------------------------------------------*
    SELECT  * FROM zib_cte_dist_d55
      INTO TABLE @DATA(t_zib_cte_dist_d55)
      FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE cd_chave_cte = @t_zib_cte_dist_ter-cd_chave_cte.

**----------------------------------------------------------------------*
    "CT-e Distribuida - Dados complementares - Campo de uso livre
**----------------------------------------------------------------------*
    SELECT  * FROM zib_cte_dist_cpl
      INTO TABLE @DATA(t_zib_cte_dist_cpl)
      FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE cd_chave_cte = @t_zib_cte_dist_ter-cd_chave_cte.

    SELECT  * FROM zib_cte_dist_cvl
      INTO TABLE @DATA(t_zib_cte_dist_cvl)
      FOR ALL ENTRIES IN @t_zib_cte_dist_ter
      WHERE cd_chave_cte = @t_zib_cte_dist_ter-cd_chave_cte.

  ENDIF.

  IF t_zib_cte_dist_n55[] IS NOT INITIAL.

    REFRESH: t_n55_ch.
    LOOP AT t_zib_cte_dist_n55 INTO DATA(w_zib_cte_dist_n55).

      w_chave_acesso = w_zib_cte_dist_n55-n55_chave_acesso.

      w_n55_ch-cd_chave_cte     = w_zib_cte_dist_n55-cd_chave_cte.
      w_n55_ch-docnum_nfe       = w_zib_cte_dist_n55-docnum_nfe.
      w_n55_ch-n55_chave_acesso = w_chave_acesso.
      w_n55_ch-regio            = w_chave_acesso-regio.
      w_n55_ch-nfyear           = w_chave_acesso-nfyear.
      w_n55_ch-nfmonth          = w_chave_acesso-nfmonth.
      w_n55_ch-stcd1            = w_chave_acesso-stcd1.
      w_n55_ch-model            = w_chave_acesso-model.
      w_n55_ch-serie            = w_chave_acesso-serie.
      w_n55_ch-nfnum9           = w_chave_acesso-nfnum9.
      w_n55_ch-docnum9          = w_chave_acesso-docnum9.
      w_n55_ch-cdv              =  w_chave_acesso-cdv.

      w_n55_ch-branch           = w_zib_cte_dist_n55-branch.
      w_n55_ch-parid            = w_zib_cte_dist_n55-parid.
      w_n55_ch-parvw            = w_zib_cte_dist_n55-parvw.

      w_n55_ch-vbeln_vl         = w_zib_cte_dist_n55-vbeln_vl.

      APPEND w_n55_ch TO t_n55_ch.
      CLEAR: w_chave_acesso, w_n55_ch.

    ENDLOOP.

  ELSE.

    MESSAGE 'Nenhum registro encontrado!'
    TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  " Cabeçalho nota fiscal
  IF t_n55_ch[] IS NOT INITIAL.

    SORT t_n55_ch BY parvw.
    DATA(t_nfpropria) = t_n55_ch[].
    DELETE t_nfpropria WHERE parvw <> 'RE'.

    SORT t_n55_ch BY parvw.
    DATA(t_nfterceiro) = t_n55_ch[].

    " Inicio CS2021000837 - ZMM0172 - Consulta de documentos (Notas São Francisco do Sul) / Anderson Oenning
    DELETE t_nfterceiro WHERE parvw EQ 'RE'.
*    DELETE t_nfterceiro WHERE parvw <> 'LF'.
    " Fim CS2021000837 - ZMM0172 - Consulta de documentos (Notas São Francisco do Sul) / Anderson Oenning
  ENDIF.

  " Cabeçalho da nota fiscal
  IF t_nfpropria[] IS NOT INITIAL.

    SELECT docnum, nfenum FROM j_1bnfdoc
      INTO TABLE @DATA(t_doc)
      FOR ALL ENTRIES IN @t_nfpropria
      WHERE  docnum = @t_nfpropria-docnum_nfe.

    SELECT docnum, pesosaida, datatransb, pesotransb  FROM zlest0039
      INTO TABLE @DATA(t_zlest0039)
      FOR ALL ENTRIES IN @t_nfpropria
      WHERE docnum = @t_nfpropria-docnum_nfe.

  ENDIF.

  IF t_n55_ch[] IS NOT INITIAL.

    DATA: r_parvw TYPE RANGE OF parvw.

    r_parvw = VALUE #( ( sign = 'I' option = 'EQ' low = 'PC' ) ).
*                         ( sign = 'I' option = 'EQ' low = 'LR' )
*                         ( sign = 'I' option = 'EQ' low = 'Z1' ) ).

    SELECT vbeln, posnr, parvw, lifnr, adrnr,  lzone FROM vbpa
    INTO TABLE @DATA(t_vbpa)
      FOR ALL ENTRIES IN @t_n55_ch
    WHERE vbeln  = @t_n55_ch-vbeln_vl
      AND parvw IN @r_parvw.

    IF t_vbpa[] IS NOT INITIAL.

*      SORT t_vbpa BY parvw.
*      DATA(t_vbpa_pc) = t_vbpa.
*      DELETE t_vbpa_pc WHERE parvw <> 'PC'.

      "Endereços (administração de endereços central)
      SELECT addrnumber, date_from, nation, taxjurcode, city1 FROM adrc
        INTO TABLE @DATA(t_adrc)
        FOR ALL ENTRIES IN @t_vbpa
        WHERE addrnumber = @t_vbpa-adrnr.

    ENDIF.

  ENDIF.

  " Dados da nota de remessa por conta e ordem de terceiros
  IF t_nfterceiro[] IS NOT INITIAL.

    SELECT centro_comprador, nr_nf, serie, nr_nf_propria, cod_cliente, docnum FROM zlest0041
      INTO TABLE @DATA(t_zlest0041)
      FOR ALL ENTRIES IN @t_nfterceiro
      WHERE centro_comprador = @t_nfterceiro-branch
        AND cod_cliente      = @t_nfterceiro-parid
        AND nr_nf            = @t_nfterceiro-nfnum9
        AND serie            = @t_nfterceiro-serie.

  ENDIF.

  "Elimina do ALV Nota de terceiros
  DELETE t_zib_cte_dist_n55 WHERE parvw = 'LF'.

  LOOP AT t_zib_cte_dist_ter INTO DATA(w_zib_cte_dist_ter).

    LOOP AT t_zib_cte_dist_n55 INTO w_zib_cte_dist_n55
                               WHERE cd_chave_cte = w_zib_cte_dist_ter-cd_chave_cte.

      MOVE-CORRESPONDING w_zib_cte_dist_ter TO w_rateio.

      "Nome filial CTe
      READ TABLE t_t001w INTO DATA(w_t001w)
                                     WITH KEY werks = w_zib_cte_dist_ter-f_tomadora.
      IF sy-subrc IS INITIAL.
        w_rateio-f_name1 =  w_t001w-name1.
      ENDIF.

      "Nome forn. emissor
      READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = w_zib_cte_dist_ter-p_emissor.
      IF sy-subrc IS INITIAL.
        w_rateio-p_emi_name1 =  w_lfa1-name1.
      ENDIF.

      w_rateio-n55_chave_acesso =  w_zib_cte_dist_n55-n55_chave_acesso.
      w_rateio-tknum            =  w_zib_cte_dist_n55-tknum.
      w_rateio-fknum            =  w_zib_cte_dist_n55-fknum.

      "Nota Própria
      READ TABLE t_nfpropria INTO DATA(w_nfpropria) WITH KEY cd_chave_cte = w_zib_cte_dist_n55-cd_chave_cte
                                                              docnum_nfe  = w_zib_cte_dist_n55-docnum_nfe
                                                              parvw       = 'RE'.
      IF sy-subrc IS INITIAL.
        READ TABLE t_doc INTO DATA(w_doc) WITH KEY docnum = w_nfpropria-docnum_nfe.
        IF sy-subrc IS INITIAL.
          w_rateio-nf_propria = w_doc-nfenum.
        ENDIF.

        "Peso rateio com base na nota própria
        READ TABLE t_zib_cte_dist_d55 INTO DATA(w_zib_cte_dist_d55)
                                                    WITH KEY cd_chave_cte     = w_nfpropria-cd_chave_cte
                                                             n55_chave_acesso = w_nfpropria-n55_chave_acesso.
        IF sy-subrc IS INITIAL.
          w_rateio-qtdade = w_zib_cte_dist_d55-valr_peso_rate * 1000.
        ENDIF.

      ENDIF.

      "Comparativo de rateios e chegadas
      READ TABLE t_zlest0039 INTO DATA(w_zlest0039) WITH KEY docnum = w_nfpropria-docnum_nfe.
      IF sy-subrc IS INITIAL.
        w_rateio-pesosaida = w_zlest0039-pesosaida.
        w_rateio-datatransb = w_zlest0039-datatransb.
        w_rateio-pesotransb = w_zlest0039-pesotransb.
      ENDIF.

      "Nota Terceiro
      READ TABLE t_zlest0041 INTO DATA(w_zlest0041) WITH KEY nr_nf_propria    = w_rateio-nf_propria.

      IF sy-subrc IS INITIAL.

        w_rateio-nf_terceiro =  w_zlest0041-nr_nf.

        READ TABLE t_nfterceiro INTO DATA(w_nfterceiro) WITH KEY cd_chave_cte = w_nfpropria-cd_chave_cte
                                                                 branch = w_zlest0041-centro_comprador
                                                                 parid  = w_zlest0041-cod_cliente
                                                                 nfnum9 = w_zlest0041-nr_nf
                                                                 serie  = w_zlest0041-serie
                                                                 parvw  = 'LF'.

        IF sy-subrc IS INITIAL.

          "Peso rateio com base na nota de terceiro
          READ TABLE t_zib_cte_dist_d55 INTO w_zib_cte_dist_d55
                                                      WITH KEY cd_chave_cte     = w_nfterceiro-cd_chave_cte
                                                               n55_chave_acesso = w_nfterceiro-n55_chave_acesso.
          IF sy-subrc IS INITIAL.
            w_rateio-qtdade = w_zib_cte_dist_d55-valr_peso_rate * 1000."Converte para KG
          ENDIF.

        ENDIF.
      ELSE.
        " Inicio CS2021000837 - ZMM0172 - Consulta de documentos (Notas São Francisco do Sul) / Anderson Oenning
        IF w_zib_cte_dist_n55-parvw IS INITIAL.
          "Peso rateio com base na nota de terceiro
          READ TABLE t_zib_cte_dist_d55 INTO w_zib_cte_dist_d55
                                                      WITH KEY cd_chave_cte     = w_zib_cte_dist_n55-cd_chave_cte
                                                               n55_chave_acesso = w_zib_cte_dist_n55-n55_chave_acesso.
          IF sy-subrc IS INITIAL.
            w_rateio-qtdade = w_zib_cte_dist_d55-valr_peso_rate * 1000."Converte para KG
          ENDIF.
        ENDIF.
        " Inicio CS2021000837 - ZMM0172 - Consulta de documentos (Notas São Francisco do Sul) / Anderson Oenning
      ENDIF.

      "Busca Ponto de coleta
      READ TABLE t_vbpa INTO DATA(w_vbpa_pc) WITH KEY vbeln = w_zib_cte_dist_n55-vbeln_vl
                                                      parvw = 'PC'.
      IF sy-subrc IS INITIAL.

*        w_rateio-ponto_coleta = w_vbpa_pc-lifnr.

        READ TABLE t_adrc INTO DATA(w_adrc) WITH KEY addrnumber = w_vbpa_pc-adrnr.
        IF sy-subrc IS  INITIAL.
          w_rateio-city1      = w_adrc-city1.      "  ( cidade do parceiro PC)
          w_rateio-taxjurcode = w_adrc-taxjurcode. " ( Domicilio Fiscal do Parceiro PC
        ENDIF.

      ENDIF.

      "seleção para tarifa cobrada
      READ TABLE t_zib_cte_dist_cvl INTO DATA(w_zib_cte_dist_cvl)
                                 WITH KEY cd_chave_cte =  w_zib_cte_dist_ter-cd_chave_cte
                                          nome_componente = 'Tarifa'.

      IF sy-subrc IS INITIAL.
        w_rateio-tarifa_cobrada = w_zib_cte_dist_cvl-valr_componente.
      ENDIF.

      "Quantidade Rateio
      w_rateio-saldo = w_rateio-pesotransb - w_rateio-qtdade.

      "Vagão
      READ TABLE t_zib_cte_dist_cpl INTO DATA(w_zib_cte_dist_cpl_vg)
                                                        WITH KEY cd_chave_cte = w_zib_cte_dist_ter-cd_chave_cte
                                                                 ds_campo     = 'VAGÃO:'.
      IF sy-subrc IS INITIAL.

        READ TABLE t_zib_cte_dist_cpl INTO DATA(w_zib_cte_dist_cpl)
                                                          WITH KEY cd_chave_cte = w_zib_cte_dist_ter-cd_chave_cte
                                                                   ds_campo     = 'SÉRIE VAGÃO:'.
        IF sy-subrc IS INITIAL.
          CONCATENATE 'SÉRIE VAGÃO: ' w_zib_cte_dist_cpl-ds_texto
                      'VAGÃO: ' w_zib_cte_dist_cpl_vg-ds_texto INTO w_rateio-ds_texto SEPARATED BY space.
        ENDIF.

      ENDIF.

      APPEND w_rateio TO t_saida_rateio.
      CLEAR: w_rateio,  w_zib_cte_dist_cpl, w_zib_cte_dist_cpl_vg,
             w_zib_cte_dist_d55, w_zlest0039, w_zlest0041, w_nfpropria, w_nfterceiro.

    ENDLOOP.

    CLEAR w_zib_cte_dist_n55.

  ENDLOOP.

  " Restringir chave de acesso
  IF s_chnfe[] IS NOT INITIAL.
    DELETE t_saida_rateio WHERE n55_chave_acesso NOT IN s_chnfe.
  ENDIF.

  "Restringir Número NF própria
  IF s_numnfe[] IS NOT INITIAL.
    DELETE t_saida_rateio WHERE nf_propria NOT IN s_numnfe.
  ENDIF.

  "Restringir Número NF Terceiro
  IF s_numnft[] IS NOT INITIAL.
    DELETE t_saida_rateio WHERE nf_terceiro NOT IN s_numnft.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_DESCR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_ajuste_descr_campos_rateio  CHANGING pt_fcat TYPE lvc_t_fcat.


  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

    CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.

    CASE <fs_fcat>-fieldname.
      WHEN 'E_TOMADORA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Empresa'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'F_TOMADORA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Filial'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'F_NAME1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome da Filial'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'EMIT_CNPJ'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'CNPJ Fornecedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'P_EMISSOR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Cód. Forn.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'P_EMI_NAME1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome do Fornecedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'NUMR_CTE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nr. CT-e'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'CD_CHAVE_CTE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Chave CT-e'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'N55_CHAVE_ACESSO'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Chave NF-e'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'DT_EMISSAO'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Data Emissão Ct-e'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'NF_PROPRIA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'NF-e própria'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'NF_TERCEIRO'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'NF Terceiro'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'PESOSAIDA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Qtdade Saída'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'DATATRANSB'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Dt. Descarga'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'PESOTRANSB'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Qtdade Descarga'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'QTDADE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Qtdade Rateio'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'SALDO'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Saldo'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'TKNUM'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Doc. Transp. Fer.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'FKNUM'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Custo Transp. Fer.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'DS_TEXTO'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Vagão '.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'CITY1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Município Origem '.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'TAXJURCODE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Dom_Mun_Origem'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'TARIFA_COBRADA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Tarifa Cobrada'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_DESCR_CAMPOS_DIVERG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM zf_ajuste_descr_campos_diverg  CHANGING pt_fcat TYPE lvc_t_fcat.

  CONSTANTS: BEGIN OF gc,
               amarelo TYPE lvc_s_colo-col      VALUE '3',
               padrao  TYPE lvc_s_colo-col      VALUE '2',
               verde   TYPE lvc_s_colo-col      VALUE '5',
               int     TYPE lvc_s_colo-int      VALUE '0',
               inv     TYPE lvc_s_colo-inv      VALUE '0',
             END OF gc.

  DATA: w_col   TYPE lvc_s_scol,
        w_color TYPE lvc_s_colo.

  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

    CLEAR: <fs_fcat>-scrtext_l, <fs_fcat>-scrtext_m, <fs_fcat>-scrtext_s.

    CASE <fs_fcat>-fieldname.
      WHEN 'STATUS'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Status'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'E_TOMADORA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Empresa'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'F_TOMADORA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Filial'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'F_NAME1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome da Filial'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'EMIT_CNPJ'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'CNPJ Fornecedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'DT_EMISSAO'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Data Emissão Ct-e'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'P_EMISSOR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Cód. Forn.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'P_EMI_NAME1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome do Fornecedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'NUMR_CTE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nr. CT-e'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'NUMR_SERIE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Série'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'TARIFA_COBRADA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Tarifa Cobrada'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'QT_CARGA_CTE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Qtde Origem'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'VALOR_PRESTACAO'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Vlr. CTe'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'ZMATNR_MERC'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Material'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'MAKTX'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Descr. Material'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'MATKL'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Gp. Material'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'WGBEZ60'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Desc. Gp. Material'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'MUNI_COLETA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Município de coleta'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'MUNI_ENTREGA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Município de entrega'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'KBETR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Tarifa Sap'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'COD_EXP'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Cód. Expedidor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'EXPEDIDOR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'CNPJ/CPF Expedidor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'EXPED_RSOCIAL'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome expedidor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'MUNI_EXPED'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Município/UF expedidor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'COD_REC'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Cód Recebedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'RECEBEDOR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'CNPJ/CPF Recebedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'RECEB_RSOCIAL'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Nome recebedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'MUNI_RECEB'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Município/UF recebedor'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'CD_CHAVE_CTE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Chave Cte'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'PESO_CHEGADA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Qtde Chegada'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'DT_CHEGADA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Dt Chegada'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

      WHEN 'TIPO_TK'."*
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Tipo TK'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = <fs_fcat>-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.

      WHEN 'SHTYP'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Tp.transp.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = <fs_fcat>-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.

      WHEN 'LZONE_LR'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Zona part.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = <fs_fcat>-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.

      WHEN 'LZONE_Z1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Zona cheg.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = <fs_fcat>-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.

      WHEN 'LOCAL_EXPED'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Loc.exped.'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = <fs_fcat>-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.

      WHEN 'COD_POSTAL'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'CdPostP'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = <fs_fcat>-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.

      WHEN 'DATA_REF'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Data Referência'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.

        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = <fs_fcat>-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.

      WHEN 'CITY1'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Município Origem '.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'TAXJURCODE'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Dom_Mun_Origem'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN 'TARIFA_COBRADA'.
        <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = 'Tarifa Cobrada'.
        <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
      WHEN OTHERS.
        <fs_fcat>-no_out = 'X'.
        <fs_fcat>-tech = 'X'.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_TARIFA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_DIVERG_KBETR  text
*      <--P_IF  text
*      <--P_W_ZIB_CTE_DIST_TER_EXPED_TP_DO  text
*      <--P_=  text
*      <--P_'1'  text
*----------------------------------------------------------------------*
FORM zf_buscar_tarifa USING p_emissor
                            p_ponto_coleta
                            p_shtyp
                            p_lzone_lr
                            p_lzone_z1
                            p_data_ref
                            p_local_exped
                            p_zmatnr_merc
                     CHANGING p_kbetr p_tipo_tk p_cod_postal.

  TRY.
      zcl_calc_frete=>get_cidade_tabela_mesorregiao(
         EXPORTING
           i_agente_frete   =  CONV #( p_emissor )
           i_ponto_coleta   =  CONV #( p_ponto_coleta )
         RECEIVING
           r_id_cidade_base =  DATA(l_id_cidade_base)   ).

      p_cod_postal = l_id_cidade_base.

    CATCH zcx_calc_frete.
  ENDTRY.
*-------------------------------------------------------------------
* Buscar tarifa - A910 – Tp.transp./ForncServ./Zona part./Zona cheg.
*-------------------------------------------------------------------
  TRY.
      "Para tabela A910
      zcl_calc_frete=>get_valor_frete(
        EXPORTING
          i_tdlnr           =  CONV #( p_emissor )
          i_shtyp           =  CONV #( p_shtyp )
          i_lzonea          =  CONV #( p_lzone_lr )
          i_lzonez          =  CONV #( p_lzone_z1 )
          i_data_referencia =  CONV #( p_data_ref )
        IMPORTING
          e_kbetr           =  DATA(l_kbetr) ).

      p_kbetr = l_kbetr.
      p_tipo_tk = 'Tp.transp./ForncServ./Zona part./Zona cheg.'.

    CATCH zcx_calc_frete.
  ENDTRY.

  CHECK l_kbetr IS INITIAL.

*-------------------------------------------------------------------
* Buscar tarifa - A933 -  Tp.transp./ForncServ./Zona part./Zona cheg./Loc.exped.
*-------------------------------------------------------------------
  IF p_local_exped IS NOT INITIAL.

    TRY.

        "Para tabela A933
        zcl_calc_frete=>get_valor_frete(
          EXPORTING
            i_tdlnr           =  CONV #( p_emissor )
            i_shtyp           =  CONV #( p_shtyp )
            i_lzonea          =  CONV #( p_lzone_lr )
            i_lzonez          =  CONV #( p_lzone_z1 )
            i_vstel           =  CONV #( p_local_exped )
            i_data_referencia =  CONV #( p_data_ref )
          IMPORTING
            e_kbetr           =  l_kbetr ).

        p_kbetr = l_kbetr.
        p_tipo_tk = 'Tp.transp./ForncServ./Zona part./Zona cheg./Loc.exped.'.

      CATCH zcx_calc_frete.
    ENDTRY.

  ENDIF.

  CHECK l_kbetr IS INITIAL.

*-------------------------------------------------------------------
* Buscar tarifa - A934 - Tp.transp./ForncServ./Zona part./Zona cheg./CdPostPart
*-------------------------------------------------------------------
  IF l_id_cidade_base IS NOT INITIAL.

    TRY.

        "Para tabela A934
        zcl_calc_frete=>get_valor_frete(
          EXPORTING
            i_tdlnr           =  CONV #( p_emissor )
            i_shtyp           =  CONV #( p_shtyp )
            i_lzonea          =  CONV #( p_lzone_lr )
            i_lzonez          =  CONV #( p_lzone_z1 )
            i_id_cidade_base  =  l_id_cidade_base
            i_data_referencia =  CONV #( p_data_ref )
          IMPORTING
            e_kbetr           =  l_kbetr ).

        p_kbetr = l_kbetr.
        p_tipo_tk = 'Tp.transp./ForncServ./Zona part./Zona cheg./CdPostPart'.

      CATCH zcx_calc_frete.
    ENDTRY.

  ENDIF.

  CHECK l_kbetr IS INITIAL.

*-------------------------------------------------------------------
* Buscar tarifa - A938 - Tp.transp./ForncServ./Zona part./Zona cheg./Material/CdPostP
*-------------------------------------------------------------------
  IF p_zmatnr_merc IS NOT INITIAL AND l_id_cidade_base IS NOT INITIAL.

    TRY.

        "Para tabela A938
        zcl_calc_frete=>get_valor_frete(
          EXPORTING
            i_tdlnr           =  CONV #( p_emissor )
            i_shtyp           =  CONV #( p_shtyp )
            i_lzonea          =  CONV #( p_lzone_lr )
            i_lzonez          =  CONV #( p_lzone_z1 )
            i_matnr           =  CONV #( p_zmatnr_merc )
            i_id_cidade_base  =  l_id_cidade_base
            i_data_referencia =  CONV #( p_data_ref )
          IMPORTING
            e_kbetr           =  l_kbetr ).

        p_kbetr = l_kbetr.
        p_tipo_tk = 'Tp.transp./ForncServ./Zona part./Zona cheg./Material/CdPostP'.

      CATCH zcx_calc_frete.
    ENDTRY.

  ENDIF.

ENDFORM.
