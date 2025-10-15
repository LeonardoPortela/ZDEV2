*&---------------------------------------------------------------------*
*& Report zppsf_ordem_producao_bio_print
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppsf_ordem_producao_bio_print.

DATA: ls_header     TYPE zpp_prod_bio_header,
      lt_formulacao TYPE zpp_prod_bio_item_t,
      lt_embalagens TYPE zpp_prod_bio_item_t.

*--------------------------------------------------------------------*
* Variáveis auxiliares
*--------------------------------------------------------------------*
DATA: lv_reg_mapa TYPE atinn,
      lv_ativo    TYPE atinn,
      lt_emb_cat  TYPE STANDARD TABLE OF mtpos_mara,
      lv_rsnum    TYPE rsnum,
      lv_cuobj    TYPE cuobj,
      lv_low      TYPE tvarvc-low,
      lt_tab_low  TYPE TABLE OF tvarvc-low,
      lv_mhdhb    TYPE mhdhb.

*--------------------------------------------------------------------*
* Iniciar variáveis
*--------------------------------------------------------------------*
INITIALIZATION.

* === Interface standard de ordens de produção ===
  INCLUDE ppcoincl.

  IMPORT caufvd_tab FROM MEMORY ID 'PPT'.

  READ TABLE caufvd_tab INDEX 1 INTO caufvd.

  DATA(p_aufnr) = caufvd-aufnr.

*--------------------------------------------------------------------*
* Busca de parâmetros de TVARV
*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_tvarv_value USING 'ZPP_IMPPRODORD_REGMAPA' CHANGING lv_low.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = lv_low
    IMPORTING
      output = lv_reg_mapa.

  PERFORM get_tvarv_value USING 'ZPP_IMPPRODORD_ATIVO'   CHANGING lv_low.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = lv_low
    IMPORTING
      output = lv_ativo.

  PERFORM get_tvarv_table USING 'ZPP_IMPPRODORD_EMB'     CHANGING lt_tab_low.
  lt_emb_cat[] = lt_tab_low[].

*--------------------------------------------------------------------*
* Cabeçalho
*--------------------------------------------------------------------*
  PERFORM get_header USING p_aufnr lv_reg_mapa lv_ativo CHANGING ls_header lv_rsnum lv_mhdhb.

*--------------------------------------------------------------------*
* Itens
*--------------------------------------------------------------------*
  PERFORM get_items USING lv_rsnum lt_emb_cat CHANGING lt_formulacao lt_embalagens.

*--------------------------------------------------------------------*
* Calcula validade
*--------------------------------------------------------------------*
  ls_header-validade = ls_header-dt_envase + lv_mhdhb.

*--------------------------------------------------------------------*
* Chama o SmartForms
*--------------------------------------------------------------------*
  DATA: lv_fm_name TYPE rs38l_fnam,
        ls_control TYPE ssfctrlop,
        ls_output  TYPE ssfcompop.
  " Configuração para imprimir direto
  CLEAR: ls_control, ls_output.
  ls_control-no_dialog = 'X'.   " Não exibir tela de spool
  ls_control-preview   = 'X'.   " exibir preview
  ls_output-tdimmed    = 'X'.   " Impressão imediata
  ls_output-tdnewid    = 'X'.   " Novo spool

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZPPSF_ORDEM_PRODUCAO_BIO'
    IMPORTING
      fm_name            = lv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-sysid = 'DEV'.
    ls_header-material = '123456789'.
    ls_header-mat_desc = 'Descricao material'.
    ls_header-ativo = '123456-54'.
    ls_header-dt_inicio = '20250101'.
    ls_header-dt_envase = '20250202'.
    ls_header-validade = '20251231'.
    ls_header-quantidade = '5000'.
    ls_header-aufnr = '98765412'.
    ls_header-REG_MAPA = '654852'.
    ls_header-un_med = 'KG'.
    ls_header-cidade_estado = 'Cuiabá - MT'.
    ls_header-empresa = 'AMAGGI'.
  ENDIF.

  IF sy-subrc = 0.
    CALL FUNCTION lv_fm_name
      EXPORTING
        control_parameters = ls_control
        output_options     = ls_output
        user_settings      = space
        is_header          = ls_header
        it_formulacao      = lt_formulacao
        it_embalagens      = lt_embalagens.
  ELSE.
    MESSAGE 'Erro ao localizar o SmartForms.' TYPE 'E'.
  ENDIF.

*--------------------------------------------------------------------*
* Form GET_TVARV_VALUE
*--------------------------------------------------------------------*
FORM get_tvarv_value USING    pv_name TYPE tvarvc-name
                     CHANGING pv_value TYPE tvarvc-low.
  SELECT SINGLE low
    INTO pv_value
    FROM tvarvc
    WHERE name = pv_name.

ENDFORM.

*--------------------------------------------------------------------*
* Form GET_TVARV_TABLE
*--------------------------------------------------------------------*
FORM get_tvarv_table USING    pv_name TYPE tvarvc-name
                     CHANGING pt_values TYPE STANDARD TABLE.
  SELECT low
    INTO TABLE pt_values
    FROM tvarvc
    WHERE name = pv_name
      AND type = 'S'. "Seleção (range)
ENDFORM.

*--------------------------------------------------------------------*
* Form GET_HEADER
*--------------------------------------------------------------------*
FORM get_header USING    pv_aufnr TYPE aufnr
                          pv_reg_mapa TYPE atinn
                          pv_ativo    TYPE atinn
                CHANGING ps_header TYPE zpp_prod_bio_header
                          pv_rsnum   TYPE rsnum
                          pv_mhdhb   TYPE mhdhb.

  DATA: ls_caufv TYPE caufv,
        lv_bwkey TYPE bwkey,
        lv_regio TYPE regio,
        lv_bukrs TYPE bukrs,
        lv_persn TYPE ad_persnum,
        lv_cuobj TYPE cuobj.

  " Dados da ordem
  SELECT SINGLE * INTO ls_caufv
    FROM caufv
    WHERE aufnr = pv_aufnr.

  ps_header-aufnr     = ls_caufv-aufnr.
  ps_header-dt_inicio = ls_caufv-gstrp.
  ps_header-dt_envase = ls_caufv-gltrp.

  ps_header-quantidade = trunc( ls_caufv-bmenge ).
  SHIFT ps_header-quantidade LEFT DELETING LEADING space.
  CONDENSE ps_header-quantidade NO-GAPS.

  ps_header-un_med    = ls_caufv-bmeins.

  pv_rsnum = ls_caufv-rsnum.

  " Empresa e cidade/estado
  SELECT SINGLE bwkey ort01 regio
  INTO (lv_bwkey, ps_header-cidade_estado, lv_regio )
    FROM t001w
    WHERE werks = ls_caufv-werks.
  CONCATENATE ps_header-cidade_estado '-' lv_regio INTO ps_header-cidade_estado SEPARATED BY space.

  SELECT SINGLE bukrs INTO lv_bukrs
    FROM t001k
    WHERE bwkey = lv_bwkey.

  SELECT SINGLE butxt INTO ps_header-empresa
    FROM t001
    WHERE bukrs = lv_bukrs.

  " Lote
  SELECT SINGLE charg INTO ps_header-lote
    FROM afpo
    WHERE aufnr = ls_caufv-aufnr.

  IF sy-sysid = 'DEV'AND ps_header-lote IS INITIAL.
    ps_header-lote = '2025DF0014'.
  ENDIF.


  " Descrição do material
  SELECT SINGLE maktg INTO ps_header-mat_desc
    FROM makt
    WHERE matnr = ls_caufv-plnbez
      AND spras = sy-langu.

  ps_header-material = ls_caufv-plnbez.

  " Reg MAPA e Ativo
*  SELECT SINGLE cuobj INTO lv_cuobj
*    FROM inob
*    WHERE objek = ls_caufv-plnbez
*      AND obtab = 'MARA'
*      AND klart = '001'.

  SELECT SINGLE atwrt INTO ps_header-reg_mapa
    FROM ausp
    WHERE objek = ls_caufv-plnbez "lv_cuobj
      AND atinn = pv_reg_mapa
      AND atwrt <> ''.

  SELECT SINGLE atwrt INTO ps_header-ativo
    FROM ausp
    WHERE objek = ls_caufv-plnbez "lv_cuobj
      AND atinn = pv_ativo
      AND atwrt <> ''.

  " Usuário impressão
  SELECT SINGLE persnumber INTO lv_persn
    FROM usr21
    WHERE bname = sy-uname.

  SELECT SINGLE name_text INTO ps_header-usuario
    FROM adrp
    WHERE persnumber = lv_persn.

  " Prazo de validade
  SELECT SINGLE mhdhb INTO pv_mhdhb
    FROM mara
    WHERE matnr = ls_caufv-plnbez.

ENDFORM.

*--------------------------------------------------------------------*
* Form GET_ITEMS
*--------------------------------------------------------------------*
FORM get_items USING    pv_rsnum TYPE rsnum
                         pt_emb_cat TYPE STANDARD TABLE
               CHANGING pt_formulacao TYPE STANDARD TABLE
                        pt_embalagens  TYPE STANDARD TABLE.

  DATA: lt_resb  TYPE TABLE OF resb,
        ls_item  TYPE zpp_prod_bio_item,
        lv_matkl TYPE matkl.

  SELECT *
    INTO TABLE @lt_resb
    FROM resb
    WHERE rsnum = @pv_rsnum
      AND xloek = ''
      AND bdmng <> 0.

  LOOP AT lt_resb INTO DATA(ls_resb).
    " Descrição
    SELECT SINGLE maktg INTO ls_item-maktg
      FROM makt
      WHERE matnr = ls_resb-matnr
        AND spras = sy-langu.

    " Categoria do material
    SELECT SINGLE matkl INTO lv_matkl
      FROM mara
      WHERE matnr = ls_resb-matnr.

    ls_item-matnr = ls_resb-matnr.


    IF ls_resb-bdmng = trunc( ls_resb-bdmng ).
      ls_item-bdmng = |{ ls_resb-bdmng DECIMALS = 0 }|.
    ELSE.
      ls_item-bdmng = |{ ls_resb-bdmng DECIMALS = 3 }|.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '.' IN ls_item-bdmng WITH ','.

    SHIFT ls_item-bdmng LEFT DELETING LEADING space.
    CONDENSE ls_item-bdmng NO-GAPS.

    ls_item-meins = ls_resb-meins.
    ls_item-charg = ls_resb-charg.

    " Verifica se é embalagem
    READ TABLE pt_emb_cat WITH KEY table_line = lv_matkl TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      APPEND ls_item TO pt_embalagens.
    ELSE.
      APPEND ls_item TO pt_formulacao.
    ENDIF.
  ENDLOOP.

  IF sy-sysid = 'DEV'.
    pt_embalagens[] = pt_formulacao[].
  ENDIF.


ENDFORM.
