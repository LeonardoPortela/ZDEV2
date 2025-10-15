*&--------------------------------------------------------------------------------&*
*&                        AMAGGI                                                  &*
*&--------------------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                              &*
*& Autor....: Jaime Tassoni                                                       &*
*& Data.....: 19.02.2024                                                          &*
*& Descrição: Criação do docnum dos Ct-e de subcontratação emitidos por Terceiros &*
*&--------------------------------------------------------------------------------&*
REPORT zlesr0181_job.

TABLES: mara, makt,mbew,zlest0040, zib_cte_dist_ter.

************************************************************************
* Constantes
************************************************************************
CONSTANTS: c_1(1)        TYPE c                 VALUE '1',
           c_ce(2)       TYPE c                 VALUE 'CE',
           c_57(2)       TYPE c                 VALUE '57',
           c_nf55(4)     TYPE c                 VALUE 'NF55',
           c_lf(2)       TYPE c                 VALUE 'LF',
           c_v(1)        TYPE c                 VALUE 'V',
           c_brl(3)      TYPE c                 VALUE 'BRL',
           c_________(1) TYPE c                 VALUE '_'.

************************************************************************
* types
************************************************************************
TYPES: BEGIN OF ty_zib_cte,
         cd_chave_cte    TYPE zib_cte_dist_ter-cd_chave_cte,
         e_tomadora      TYPE zib_cte_dist_ter-e_tomadora,
         f_tomadora      TYPE zib_cte_dist_ter-f_tomadora,
         toma4_cnpj      TYPE zib_cte_dist_ter-toma4_cnpj,
         valor_prestacao TYPE zib_cte_dist_ter-valor_prestacao,
         dt_emissao      TYPE zib_cte_dist_ter-dt_emissao,
         numr_serie      TYPE zib_cte_dist_ter-numr_serie,
         p_emissor       TYPE zib_cte_dist_ter-p_emissor,
         numr_cte        TYPE zib_cte_dist_ter-numr_cte,
         codg_cfop       TYPE zib_cte_dist_ter-codg_cfop,
         cst_icms        TYPE zib_cte_dist_ter-cst_icms,
         valor_base_icms TYPE zib_cte_dist_ter-valor_base_icms,
         valor_icms      TYPE zib_cte_dist_ter-valor_icms,
         docnum_cte      TYPE zib_cte_dist_ter-docnum_cte,
         bukrs           TYPE j_1bbranch-bukrs,
         branch          TYPE j_1bbranch-branch,
         inicio_uf       TYPE zib_cte_dist_ter-inicio_uf, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         termino_uf      TYPE zib_cte_dist_ter-termino_uf, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         inicio_ibge     TYPE zib_cte_dist_ter-inicio_ibge, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         termino_ibge    TYPE zib_cte_dist_ter-termino_ibge. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
TYPES: END OF ty_zib_cte.

TYPES: BEGIN OF ty_subcontra,
         empresa_toma    TYPE zib_cte_dist_ter-e_tomadora,
         filial_toma     TYPE zib_cte_dist_ter-f_tomadora,
         cnpj_toma       TYPE zib_cte_dist_ter-toma4_cnpj,
         chave_cte       TYPE zib_cte_dist_ter-cd_chave_cte,
         valor_cte       TYPE zib_cte_dist_ter-valor_prestacao,
         dt_emissao      TYPE zib_cte_dist_ter-dt_emissao,
         serie           TYPE zib_cte_dist_ter-numr_serie,
         bukrs           TYPE zib_cte_dist_ter-e_tomadora,
         branch          TYPE zib_cte_dist_ter-f_tomadora,
         parid           TYPE zib_cte_dist_ter-p_emissor,
         nr_cte          TYPE zib_cte_dist_ter-numr_cte,
         p_emissor       TYPE zib_cte_dist_ter-p_emissor,
         cfop_xml        TYPE zib_cte_dist_ter-codg_cfop,
         categ_icms      TYPE zib_cte_dist_ter-cst_icms,
         base_icms       TYPE zib_cte_dist_ter-valor_base_icms,
         vlr_icms        TYPE zib_cte_dist_ter-valor_icms,
*
         docnum_cte      TYPE zib_cte_dist_ter-docnum_cte,
         chave_cte_norm  TYPE zib_cte_dist_ter-cd_chave_cte,
         nota            TYPE zde_campos_nfe,
         docnum_cte_norm TYPE j_1bdocnum,
         valor_norm      TYPE j_1bnftot,
         tknum           TYPE vttk-tknum,
         belnr           TYPE zlest0032-belnr,
         docnum_ent      TYPE zlest0032-docnum,
         inicio_uf       TYPE zib_cte_dist_ter-inicio_uf, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         termino_uf      TYPE zib_cte_dist_ter-termino_uf, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         inicio_ibge     TYPE zib_cte_dist_ter-inicio_ibge, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         termino_ibge    TYPE zib_cte_dist_ter-termino_ibge. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
TYPES: END   OF ty_subcontra.

************************************************************************
* variaveis globais
************************************************************************
DATA: t_zib_cte_dist_ter TYPE TABLE OF ty_zib_cte,
      t_iddocant         TYPE zde_infs_doc_ant_tt,
      t_iddocantele      TYPE zde_infs_doc_ant_ele_t,
      t_j_1bbranch       TYPE TABLE OF j_1bbranch,
      t_subcontra        TYPE TABLE OF ty_subcontra,
      t_zlest0032        TYPE TABLE OF zlest0032,
      t_cria_subcont     TYPE TABLE OF ty_subcontra,
      t_value            TYPE TABLE OF rgsb4,
      t_status           TYPE zde_btcstatus_t,
      w_zib_cte_dist_ter TYPE ty_zib_cte,
      w_j_1bbranch       TYPE j_1bbranch,
      w_iddocant         TYPE zde_infs_doc_ant,
      w_iddocantele      TYPE zde_infs_doc_ant_ele,
      w_subcontra        TYPE ty_subcontra,
      w_cria_subcont     TYPE ty_subcontra,
      w_value            TYPE rgsb4,
      l_tabix            TYPE sy-tabix,
      l_tolerancia       TYPE p DECIMALS 0,
      l_perc_var         TYPE p DECIMALS 0, "char6, "#165700-05.02.2025-JT-inicio
      l_check_tolera     TYPE p DECIMALS 0, "char6, "#165700-05.02.2025-JT-inicio
      l_valor_norm       TYPE j_1bnftot,
      l_menor_vlr        TYPE j_1bnftot,
      l_maior_vlr        TYPE j_1bnftot,
      l_gerar_nf         TYPE char01,
      l_matnr_subcon     TYPE mara-matnr,
      l_dstcat           TYPE zlest0030-dstcat,
      l_iva              TYPE zlest0040-iva,
      l_cfop             TYPE zlest0030-cfop,
      l_mensagem         TYPE string,
      lc_docnum          TYPE bapi_j_1bnfdoc-docnum,
      lc_campos_nfe      TYPE zde_campos_nfe,
      l_docnum_cte       TYPE  zib_cte_dist_ter-docnum_cte,
      lc_xml_cte         TYPE zcte_xml_sefaz_auth_aj,
      l_erro             TYPE char01,
      zcl_util           TYPE REF TO zcl_util,
*
      t_return           TYPE TABLE OF bapiret2,
      w_return           TYPE bapiret2,
      t_obj_partner      TYPE TABLE OF bapi_j_1bnfnad,
      t_obj_item         TYPE TABLE OF bapi_j_1bnflin,
      t_obj_item_add     TYPE TABLE OF bapi_j_1bnflin_add,
      t_obj_item_tax     TYPE TABLE OF bapi_j_1bnfstx,
      t_obj_ot_partner   TYPE TABLE OF bapi_j_1bnfcpd,
*
      w_nfcheck          TYPE bapi_j_1bnfcheck,
      w_obj_header       TYPE bapi_j_1bnfdoc,
      w_obj_header_add   TYPE bapi_j_1bnfdoc_add,
      w_obj_partner      TYPE bapi_j_1bnfnad,
      w_obj_item         TYPE bapi_j_1bnflin,
      w_obj_item_add     TYPE bapi_j_1bnflin_add,
      w_obj_item_tax     TYPE bapi_j_1bnfstx,
      w_obj_ot_partner   TYPE bapi_j_1bnfcpd.

RANGES: r_data            FOR zib_cte_dist_ter-dt_emissao.

************************************************************************
* parametros entrada
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_chave   FOR zib_cte_dist_ter-cd_chave_cte.
  PARAMETERS     : p_data   TYPE datum  OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END   OF BLOCK b1.

************************************************************************
*  start
************************************************************************
START-OF-SELECTION.

  FREE: t_status.

  APPEND 'R' TO t_status.

*---------------------------------------------
* se tem Job ativo, abandona
*---------------------------------------------
  TRY .
      zcl_job=>get_job_programa_execucao(
        EXPORTING
          i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
          i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
          i_status     = t_status    " Status de Jobs
        IMPORTING
          e_quantidade = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd > 1.
    EXIT.
  ENDIF.

*-selecao -------------------------------
  PERFORM f_selecao_dados.

  IF t_zib_cte_dist_ter[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-002.
    RETURN.
  ENDIF.

*-processamento -------------------------
  PERFORM f_processamento.

************************************************************************
* selecao dados
************************************************************************
FORM f_selecao_dados.

  FREE: r_data.

*-------------------------
* tolerancia para gerar registro fiscal
*-------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZLESR0181_TOLERANCIA'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  READ TABLE t_value INTO w_value INDEX 1.

  l_check_tolera = COND #( WHEN sy-subrc = 0 THEN w_value-from
                                             ELSE 0 ).

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZLESR0181_MATERIAL'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  READ TABLE t_value INTO w_value INDEX 1.

  l_matnr_subcon = w_value-from.

*-------------------------
* range data
*-------------------------
  r_data-sign   = 'I'.
  r_data-option = 'BT'.
  r_data-low    = p_data - 30.
  r_data-high   = p_data - 1.
  APPEND r_data.

*-------------------------
* selecao
*-------------------------
  SELECT cd_chave_cte, e_tomadora,      f_tomadora, toma4_cnpj, valor_prestacao,
  dt_emissao,   numr_serie,      p_emissor,  numr_cte,   codg_cfop,
  cst_icms,     valor_base_icms, valor_icms, docnum_cte
    ,inicio_uf, termino_uf,inicio_ibge,termino_ibge "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
  FROM zib_cte_dist_ter
  WHERE cd_chave_cte   IN @s_chave
  AND dt_emissao     IN @r_data
  AND cd_modal        = '01'   "Rodoviário
  AND cd_tipo_servico = '1'    "Subcontratação
  AND toma4_tp_doc    = '1'   "CNPJ
  AND docnum_cte     IS INITIAL
  INTO TABLE @DATA(dados_zib_cte_dist_ter).

  MOVE-CORRESPONDING dados_zib_cte_dist_ter TO t_zib_cte_dist_ter.
  FREE: dados_zib_cte_dist_ter.

  CHECK t_zib_cte_dist_ter[] IS NOT INITIAL.

  LOOP AT t_zib_cte_dist_ter  INTO w_zib_cte_dist_ter.
    w_zib_cte_dist_ter-bukrs     = w_zib_cte_dist_ter-e_tomadora.
    w_zib_cte_dist_ter-branch    = w_zib_cte_dist_ter-f_tomadora.
    MODIFY t_zib_cte_dist_ter FROM w_zib_cte_dist_ter INDEX sy-tabix.
  ENDLOOP.

  SELECT *
    FROM j_1bbranch
    INTO TABLE @t_j_1bbranch
     FOR ALL ENTRIES IN @t_zib_cte_dist_ter
   WHERE bukrs  = @t_zib_cte_dist_ter-bukrs
     AND branch = @t_zib_cte_dist_ter-branch.

  LOOP AT t_zib_cte_dist_ter  INTO w_zib_cte_dist_ter WHERE e_tomadora IS NOT INITIAL
                                                        AND f_tomadora IS NOT INITIAL.
    l_tabix = sy-tabix.

    READ TABLE t_j_1bbranch   INTO w_j_1bbranch WITH KEY bukrs  = w_zib_cte_dist_ter-bukrs
                                                         branch = w_zib_cte_dist_ter-branch.
    IF sy-subrc <> 0.
      DELETE t_zib_cte_dist_ter INDEX l_tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

************************************************************************
*  Processamento
************************************************************************
FORM f_processamento.

  CREATE OBJECT zcl_util.

*-------------------------
* buscar chave CT-e
*-------------------------
  LOOP AT t_zib_cte_dist_ter  INTO w_zib_cte_dist_ter.

    FREE: lc_xml_cte, l_valor_norm, t_subcontra, l_gerar_nf, t_zlest0032, t_iddocantele.

    CALL FUNCTION 'Z_DETALHAMENTO_CTE_XML'
      EXPORTING
        i_chave_nfe = w_zib_cte_dist_ter-cd_chave_cte
      IMPORTING
        e_xml_cte   = lc_xml_cte
      EXCEPTIONS
        no_found    = 1
        OTHERS      = 2.

*   t_iddocantele[] = lc_xml_cte-cteproc-cte-infcte-infctenorm-docant-emidocant-iddocant[1]-iddocantele[].
    t_iddocant[] = lc_xml_cte-cteproc-cte-infcte-infctenorm-docant-emidocant-iddocant[].

    LOOP AT t_iddocant INTO w_iddocant.
      APPEND LINES OF w_iddocant-iddocantele[] TO t_iddocantele[].
    ENDLOOP.

    IF t_iddocantele[] IS INITIAL.
      l_mensagem = 'XMLs CT-e normal nao encontrados!'.
      PERFORM f_grava_log USING w_zib_cte_dist_ter-cd_chave_cte 0 'E' l_mensagem.
      CONTINUE.
    ENDIF.

*-------------------------
*---Percorre Chave CT-e do XML
*-------------------------
    LOOP AT t_iddocantele       INTO w_iddocantele.

      w_subcontra-empresa_toma     = w_zib_cte_dist_ter-e_tomadora.
      w_subcontra-filial_toma      = w_zib_cte_dist_ter-f_tomadora.
      w_subcontra-cnpj_toma        = w_zib_cte_dist_ter-toma4_cnpj.
      w_subcontra-chave_cte        = w_zib_cte_dist_ter-cd_chave_cte.
      w_subcontra-valor_cte        = w_zib_cte_dist_ter-valor_prestacao.
      w_subcontra-dt_emissao       = w_zib_cte_dist_ter-dt_emissao.
      w_subcontra-serie            = w_zib_cte_dist_ter-numr_serie.
      w_subcontra-bukrs            = w_zib_cte_dist_ter-e_tomadora.
      w_subcontra-branch           = w_zib_cte_dist_ter-f_tomadora.
      w_subcontra-parid            = w_zib_cte_dist_ter-p_emissor.
      w_subcontra-nr_cte           = w_zib_cte_dist_ter-numr_cte.
      w_subcontra-p_emissor        = w_zib_cte_dist_ter-p_emissor.
      w_subcontra-cfop_xml         = w_zib_cte_dist_ter-codg_cfop.
      w_subcontra-categ_icms       = w_zib_cte_dist_ter-cst_icms.
      w_subcontra-base_icms        = w_zib_cte_dist_ter-valor_base_icms.
      w_subcontra-vlr_icms         = w_zib_cte_dist_ter-valor_icms.
      w_subcontra-docnum_cte       = w_zib_cte_dist_ter-docnum_cte.
*
      lc_campos_nfe                = zcl_util->get_atributos_nfe( CONV #( w_iddocantele-chcte ) ).
      w_subcontra-chave_cte_norm   = w_iddocantele-chcte.
      w_subcontra-nota             = lc_campos_nfe.
      w_subcontra-nota-serie       = lc_campos_nfe-serie+2(1).

*-----Buscar o docnum do ct-e próprio
      SELECT branch
        INTO @DATA(_branch)
        FROM j_1bbranch
         UP TO 1 ROWS
       WHERE stcd1 = @lc_campos_nfe-stcd1.
      ENDSELECT.

      CHECK sy-subrc = 0.

      SELECT docnum, nftot
        INTO @DATA(_j_1bnfdoc)
        FROM j_1bnfdoc
       UP TO 1 ROWS
       WHERE direct  = '2'
         AND model   = @w_subcontra-nota-model
         AND series  = @w_subcontra-nota-serie
         AND branch  = @_branch
         AND nfenum  = @w_subcontra-nota-nfnum9
         AND cancel <> @abap_true.
      ENDSELECT.

      CHECK sy-subrc = 0.

      w_subcontra-docnum_cte_norm  = _j_1bnfdoc-docnum.
      w_subcontra-valor_norm       = _j_1bnfdoc-nftot.

*-----Verifica se para o CT-e próprio emitido foi lançada a miro e docnum de entrada
      SELECT refkey, refitm
        INTO @DATA(_j_1bnflin)
        FROM j_1bnflin
       UP TO 1 ROWS
       WHERE docnum  = @w_subcontra-docnum_cte_norm.
      ENDSELECT.

      CHECK sy-subrc = 0.

      SELECT vbelv
        INTO @DATA(_vbelv)
        FROM vbfa
       UP TO 1 ROWS
       WHERE vbeln = @_j_1bnflin-refkey
         AND posnn = @_j_1bnflin-refitm.
      ENDSELECT.

      CHECK sy-subrc = 0.

      SELECT tknum
        INTO @DATA(_tknum)
        FROM vbak
       UP TO 1 ROWS
       WHERE vbeln = @_vbelv.
      ENDSELECT.

      CHECK sy-subrc = 0.

      SELECT *
        INTO @DATA(_zlest0032)
        FROM zlest0032
       UP TO 1 ROWS
       WHERE tknum = @_tknum.
      ENDSELECT.

      IF ( sy-subrc  = 0 AND ( _zlest0032-belnr  IS INITIAL OR _zlest0032-docnum IS INITIAL ) ) OR
         ( sy-subrc <> 0 ).
        l_gerar_nf         = abap_false.
        EXIT.
      ELSE.
        APPEND _zlest0032 TO t_zlest0032.
        l_gerar_nf         = abap_true.
      ENDIF.

      l_valor_norm                 = l_valor_norm + w_subcontra-valor_norm.
*
      w_subcontra-tknum            = _tknum.
      w_subcontra-belnr            = _zlest0032-belnr.
      w_subcontra-docnum_ent       = _zlest0032-docnum.


      w_subcontra-inicio_uf   = w_zib_cte_dist_ter-inicio_uf.
      w_subcontra-termino_uf    = w_zib_cte_dist_ter-termino_uf.
      w_subcontra-inicio_ibge    = w_zib_cte_dist_ter-inicio_ibge.
      w_subcontra-termino_ibge   = w_zib_cte_dist_ter-termino_ibge.


      IF _tknum IS NOT INITIAL. "178005 Ajustes  ZFIS72 PSA
        SELECT SINGLE * FROM vfkp
         WHERE rebel = @_tknum
           AND fkpos = '000001'
           INTO  @DATA(w_vfkp).
        IF sy-subrc = 0.
          CLEAR: l_valor_norm.
          SELECT SINGLE kwert FROM prcd_elements WHERE knumv = @w_vfkp-knumv  AND kposn  = @w_vfkp-fkpos AND kappl   = 'F' AND kschl   = 'ZFRE' INTO @l_valor_norm.
        ENDIF.
      ENDIF. "178005 Ajustes  ZFIS72 PSA


      APPEND w_subcontra          TO t_subcontra.
    ENDLOOP.

*-------------------------
*-----checar miro / reg fiscal entrada
*-------------------------
    CHECK l_gerar_nf = abap_true.

*-------------------------
*-----checar tolerancia
*-------------------------
    l_menor_vlr                  = COND #( WHEN l_valor_norm <  w_subcontra-valor_cte THEN l_valor_norm
                                                                                      ELSE w_subcontra-valor_cte ).
    l_maior_vlr                  = COND #( WHEN l_valor_norm >= w_subcontra-valor_cte THEN l_valor_norm
                                                                                      ELSE w_subcontra-valor_cte ).

    l_menor_vlr                  = COND #( WHEN l_menor_vlr   = 0                     THEN '0.01'        "#165700-05.02.2025-JT
                                                                                      ELSE l_menor_vlr ).

    l_tolerancia                 = ( ( l_maior_vlr / l_menor_vlr ) - 1 ) * 100.
    l_perc_var                   = l_tolerancia.
*   CONDENSE l_perc_var.                                                                                 "#165700-05.02.2025-JT

    IF l_perc_var > l_check_tolera.
      l_mensagem = 'Ultrapassado Percentual de Tolerancia:' && l_perc_var.
      PERFORM f_grava_log USING w_subcontra-chave_cte 0 'E' l_mensagem.
      CONTINUE.
    ENDIF.

*-------------------------
*---criar registro fiscal
*-------------------------
    PERFORM f_cria_registro_fiscal CHANGING lc_docnum.

    WAIT UP TO 5 SECONDS."178005 Ajustes  ZFIS72 PSA

    CHECK lc_docnum IS NOT INITIAL.

    SELECT SINGLE * FROM j_1bnfdoc WHERE docnum = @lc_docnum AND cancel = @abap_false INTO @DATA(ok_j_1bnfdoc).  "178005 Ajustes  ZFIS72 PSA
    IF sy-subrc = 0.  "178005 Ajustes  ZFIS72 PSA
*-------------------------
*---salvar tabelas
*-------------------------
      PERFORM f_salvar_tabelas          USING lc_docnum.
    ENDIF.



  ENDLOOP.

ENDFORM.

************************************************************************
*  criar registro fiscal
************************************************************************
FORM f_cria_registro_fiscal CHANGING p_docnum.

  CREATE OBJECT zcl_util.

  FREE: t_obj_partner,  t_obj_item,       t_obj_item_add,
        t_obj_item_tax, t_obj_ot_partner, w_obj_header,
        p_docnum.

  READ TABLE t_subcontra INTO w_subcontra INDEX 1.

*-HEADER ---------------------------------------------
  w_obj_header-nftype  = c_ce.
  w_obj_header-doctyp  = c_1.
  w_obj_header-direct  = c_1.
  w_obj_header-docstat = abap_off.
  w_obj_header-docdat  = w_subcontra-dt_emissao.
  w_obj_header-pstdat  = sy-datum.
  w_obj_header-credat  = sy-datum.
  w_obj_header-manual  = abap_on.
  w_obj_header-waerk   = c_brl.
  w_obj_header-model   = c_57.
  w_obj_header-series  = w_subcontra-serie.
  w_obj_header-bukrs   = w_subcontra-bukrs.
  w_obj_header-branch  = w_subcontra-branch.
  w_obj_header-parvw   = c_lf.
  w_obj_header-parid   = w_subcontra-parid.
  w_obj_header-partyp   = 'V'.
  w_obj_header-nfe     = abap_on.
  w_obj_header-form    = space."c_nf55.
  w_obj_header-nfenum  = w_subcontra-nr_cte.
  w_obj_header-cte_strt_lct    =  |{ w_subcontra-inicio_uf } { w_subcontra-inicio_ibge }|. "178005 Ajustes  ZFIS72 PSA
  w_obj_header-cte_end_lct     = |{ w_subcontra-termino_uf } { w_subcontra-termino_ibge }|. "178005 Ajustes  ZFIS72 PSA
  w_obj_header-access_key = w_subcontra-chave_cte.

*-PARCEIRO -------------------------------------------
  w_obj_partner-parvw  = c_lf.
  w_obj_partner-parid  = w_subcontra-parid.
  w_obj_partner-partyp = c_v.
  APPEND w_obj_partner TO t_obj_partner.

*-ITENS ----------------------------------------------
  PERFORM f_busca_material CHANGING mara-matnr      mara-matkl    mara-meins             makt-maktx               l_erro.
  CHECK l_erro = abap_false.

  PERFORM f_busca_impostos CHANGING zlest0040-icms  zlest0040-ipi zlest0040-cofins zlest0040-pis l_iva l_erro.
  CHECK l_erro = abap_false.

  PERFORM f_busca_mbew USING mara-matnr CHANGING mbew-mtorg mbew-mtuse l_erro. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
  CHECK l_erro = abap_false.

*  PERFORM f_busca_cfop     CHANGING l_cfop          l_erro. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
*  CHECK l_erro = abap_false. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA

  w_obj_item-itmnum  = 10.

  IF strlen( mara-matnr ) > 18.
    w_obj_item-matnr_long = mara-matnr.
  ELSE.
    w_obj_item-matnr      = mara-matnr.
  ENDIF.

  w_obj_item-bwkey   = w_subcontra-branch.
  w_obj_item-maktx   = makt-maktx. "178005 Ajustes  ZFIS72 PSA
  w_obj_item-matkl   = mara-matkl. "178005 Ajustes  ZFIS72 PSA
  w_obj_item-netwr   = w_subcontra-valor_cte. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
  w_obj_item-matorg  = mbew-mtorg. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA ***
  w_obj_item-matuse  = mbew-mtuse. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA ***
  w_obj_item-menge   = c_1.
  w_obj_item-meins   = mara-meins.
  w_obj_item-netpr   = w_subcontra-valor_cte.
  w_obj_item-werks   = w_subcontra-branch.
  w_obj_item-itmtyp  = 1.
  w_obj_item-taxlw1  = zlest0040-icms.
  w_obj_item-taxlw2  = zlest0040-ipi.

  "w_obj_item-cfop_10 = l_cfop. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
  IF zib_cte_dist_ter-inicio_uf = zib_cte_dist_ter-termino_uf.
    w_obj_item-cfop_10 = '1360AA'. "'1360/AA'. 178005 Ajustes  ZFIS72 PSA
  ELSEIF  zib_cte_dist_ter-inicio_uf  <> zib_cte_dist_ter-termino_uf.
    w_obj_item-cfop_10 = '2360AA'. "'2360/AA'. 178005 Ajustes  ZFIS72 PSA
  ENDIF.
  w_obj_item-taxlw4  = zlest0040-cofins.
  w_obj_item-taxlw5  = zlest0040-pis.

  APPEND w_obj_item  TO t_obj_item.

*-ICMS -----------------------------------------------
  CASE l_iva.
    WHEN 'I8'.
      w_obj_item_tax-itmnum = 10.
      w_obj_item_tax-taxtyp = 'ICM3'.
      w_obj_item_tax-base   = w_subcontra-base_icms.
      w_obj_item_tax-rate   = round( val = ( w_subcontra-vlr_icms / w_subcontra-base_icms ) * 100  dec = 2 ).
      w_obj_item_tax-taxval = w_subcontra-vlr_icms.
      w_obj_item_tax-othbas = w_subcontra-valor_cte - w_subcontra-base_icms.
      APPEND w_obj_item_tax   TO t_obj_item_tax.
    WHEN 'I7'.
      w_obj_item_tax-itmnum = 10.
      w_obj_item_tax-taxtyp = 'ICM3'.
      w_obj_item_tax-othbas = w_subcontra-valor_cte.
      APPEND w_obj_item_tax   TO t_obj_item_tax.
  ENDCASE.

*-PIS ------------------------------------------------
  w_obj_item_tax-itmnum = 10.
  w_obj_item_tax-taxtyp = 'IPIS'.
  w_obj_item_tax-othbas = w_subcontra-valor_cte.
  APPEND w_obj_item_tax   TO t_obj_item_tax.

*-COFINS ---------------------------------------------
  w_obj_item_tax-itmnum = 10.
  w_obj_item_tax-taxtyp = 'ICOF'.
  w_obj_item_tax-othbas = w_subcontra-valor_cte.
  APPEND w_obj_item_tax   TO t_obj_item_tax.

*-IVA --------------------------------------------- 170396 CS2022001028 Questão Legal parte 3 PSA
  w_obj_item_add-itmnum = 10.
  w_obj_item_add-mwskz = l_iva.
  APPEND w_obj_item_add   TO t_obj_item_add.

*-NFCHECK --------------------------------------------
  w_nfcheck-chekcon     = abap_true.

*-----------------------------------------------------
*-cria registro fiscal ------------------------------
*-----------------------------------------------------
  CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      obj_header     = w_obj_header
      nfcheck        = w_nfcheck
    IMPORTING
      e_docnum       = p_docnum
    TABLES
      obj_partner    = t_obj_partner
      obj_item       = t_obj_item
      obj_item_tax   = t_obj_item_tax
      obj_ot_partner = t_obj_ot_partner
      return         = t_return
    EXCEPTIONS
      OTHERS         = 1.

  IF lc_docnum IS INITIAL.
    LOOP AT t_return INTO w_return.
      MESSAGE ID w_return-id  TYPE w_return-type
                            NUMBER w_return-number
                              WITH w_return-message_v1 w_return-message_v2 w_return-message_v3 w_return-message_v4
                              INTO l_mensagem.
      PERFORM f_grava_log    USING w_subcontra-chave_cte 0 'E' l_mensagem.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.

************************************************************************
* busca dados material
************************************************************************
FORM f_busca_material CHANGING p_matnr
                               p_matkl
                               p_meins
                               p_maktx "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
                               p_erro.

  FREE: p_matkl, p_meins, p_erro.

  p_matnr = l_matnr_subcon.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = p_matnr
    IMPORTING
      output       = p_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  SELECT SINGLE a~matnr, a~matkl, a~meins, b~maktx "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA (Adicvionado descição do material)
    INTO @DATA(_mara)
    FROM mara AS a
    INNER JOIN makt AS b ON b~matnr = a~matnr
   WHERE a~matnr = @p_matnr
    AND b~spras = 'P'.

  IF sy-subrc <> 0.
    p_erro     = abap_true.
    l_mensagem = 'Material não encontrado:' && p_matnr.
    PERFORM f_grava_log USING w_subcontra-chave_cte 0 'E' l_mensagem.
    EXIT.
  ENDIF.

  p_matkl = _mara-matkl.
  p_maktx = _mara-maktx. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA (Adicvionado descição do material)
  p_meins = _mara-meins.

ENDFORM.

************************************************************************
* busca impostos
************************************************************************
FORM f_busca_impostos CHANGING p_icms
                               p_ipi
                               p_cofins
                               p_pis
                               p_iva
                               p_erro.

  FREE: p_iva, p_icms, p_ipi, p_cofins, p_pis, p_erro.

  CASE w_subcontra-categ_icms.
    WHEN '00'  OR '10'  OR '20' OR '30' OR '70'.
      l_iva = 'I8'.
    WHEN OTHERS.
      l_iva = 'I7'.
  ENDCASE.

  SELECT SINGLE icms, ipi, cofins, pis
    INTO @DATA(_zlest0040)
    FROM zlest0040
   WHERE iva    = @l_iva
     AND fatura = 'T'.  "terceiros

  IF sy-subrc <> 0.
    p_erro     = abap_true.
    l_mensagem = 'Leis Fiscais IVA não encontrado. IVA:' && l_iva.
    PERFORM f_grava_log USING w_subcontra-chave_cte 0 'E' l_mensagem.
    EXIT.
  ENDIF.

  p_icms   = _zlest0040-icms.
  p_ipi    = _zlest0040-ipi.
  p_cofins = _zlest0040-cofins.
  p_pis    = _zlest0040-pis.
  p_iva    = l_iva.

ENDFORM.
************************************************************************
* busca MBEW
************************************************************************
FORM f_busca_mbew USING _material TYPE mara-matnr CHANGING p_mtorg p_mtuse p_erro. "170396 CS2022001028 Questão Legal parte 3 PSA

  FREE: p_mtorg, p_mtuse, p_erro.

  SELECT SINGLE * FROM mbew WHERE matnr = @_material INTO @DATA(ls_mbew).
  IF sy-subrc = 0.
    p_mtorg = ls_mbew-mtorg.
    p_mtuse = ls_mbew-mtuse.
  ELSE.
    p_erro     = abap_true.
    l_mensagem = 'Não foi possivel achar valores para os campos MTORG e MTUSE!'.
    PERFORM f_grava_log USING w_subcontra-chave_cte 0 'E' l_mensagem.
    EXIT.
  ENDIF.

ENDFORM.
************************************************************************
* busca impostos
************************************************************************
FORM f_busca_cfop     CHANGING p_cfop
                               p_erro.

  FREE: p_erro, p_cfop.

  SELECT SINGLE lifnr, regio
    INTO @DATA(_lfa1)
    FROM lfa1
   WHERE lifnr = @w_subcontra-p_emissor.
  IF sy-subrc <> 0.
    CLEAR _lfa1.
  ENDIF.

  SELECT SINGLE adrnr, industry
    INTO @DATA(_j_1bbranch)
    FROM j_1bbranch
   WHERE bukrs  = @w_subcontra-empresa_toma
     AND branch = @w_subcontra-filial_toma.
  IF sy-subrc <> 0.
    CLEAR _j_1bbranch.
  ENDIF.

  SELECT SINGLE addrnumber, region
    INTO @DATA(_adrc)
    FROM adrc
   WHERE addrnumber = @_j_1bbranch-adrnr
     AND date_to   >= @sy-datum.
  IF sy-subrc <> 0.
    CLEAR _adrc.
  ENDIF.

  IF _lfa1-regio = _adrc-region.
    l_dstcat = '0'.
  ELSE.
    l_dstcat = '1'.
  ENDIF.

  SELECT SINGLE *
    INTO @DATA(_zlest0030)
    FROM zlest0030
   WHERE direct     = @c_1
     AND dstcat     = @l_dstcat
     AND industry   = @_j_1bbranch-industry
     AND tpparceiro = @c_1
     AND vkaus      = @abap_off
     AND tdlnr      = @abap_off
     AND bukrs      = @abap_off.

  IF sy-subrc <> 0.
    p_erro     = abap_true.
    l_mensagem = 'Não Localizado CFOP  para  Direção:' && c_1 && ', Cat.Dest: ' && l_dstcat && ', Categoria: ' && _j_1bbranch-industry &&
                 'Tp.Parc: ' && c_1.
    PERFORM f_grava_log USING w_subcontra-chave_cte 0 'E' l_mensagem.
    EXIT.
  ENDIF.

  CASE w_subcontra-cfop_xml.
    WHEN '5932' OR '6932'.
      p_cfop = _zlest0030-cfop.
*     p_cfop = _zlest0030-cfop_uf_emit_dif_prest.
    WHEN OTHERS.
      p_cfop = _zlest0030-cfop_uf_emit_dif_prest.
*     p_cfop = _zlest0030-cfop.
  ENDCASE.

ENDFORM.

************************************************************************
* salvar tabelas
************************************************************************
FORM f_salvar_tabelas USING p_docnum.

  l_mensagem = 'Registro Fiscal Criado: ' && p_docnum.

  PERFORM f_grava_log     USING w_subcontra-chave_cte p_docnum 'S' l_mensagem.

  UPDATE zib_cte_dist_ter   SET docnum_cte   = p_docnum
                          WHERE cd_chave_cte = w_subcontra-chave_cte.

  LOOP AT t_subcontra      INTO w_subcontra.
    UPDATE zib_cte_dist_ter SET docnum_cte_subcont = p_docnum
                          WHERE cd_chave_cte       = w_subcontra-chave_cte_norm.
  ENDLOOP.

  LOOP AT t_zlest0032      INTO DATA(w_zlest0032).
    UPDATE zlest0032        SET docnum_sub   = p_docnum
                          WHERE tknum        = w_zlest0032-tknum
                            AND fknum        = w_zlest0032-fknum
                            AND ebeln        = w_zlest0032-ebeln
                            AND ebelp        = w_zlest0032-ebelp
                            AND lblni        = w_zlest0032-lblni.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.

************************************************************************
* gravar log
************************************************************************
FORM f_grava_log USING p_chave_cte
                       p_docnum
                       p_status
                       p_mensagem.

  DATA: l_timestampl TYPE timestampl,
        w_zlest0246  TYPE zlest0246.

  DO.
    GET TIME STAMP FIELD l_timestampl.

    SELECT SINGLE cd_chave_cte
      INTO @DATA(_cd_chave_cte)
      FROM zlest0246
     WHERE cd_chave_cte = @p_chave_cte
       AND seq          = @l_timestampl.

    CHECK sy-subrc <> 0.

    w_zlest0246-mandt         = sy-mandt.
    w_zlest0246-cd_chave_cte  = p_chave_cte.
    w_zlest0246-seq           = l_timestampl.
    w_zlest0246-docnum        = p_docnum.
    w_zlest0246-status_msg    = p_status.
    w_zlest0246-mensagem      = p_mensagem.
    w_zlest0246-data_reg      = sy-datum.
    w_zlest0246-hora_reg      = sy-uzeit.
    w_zlest0246-user_reg      = sy-uname.
    MODIFY zlest0246       FROM w_zlest0246.

    COMMIT WORK.
    EXIT.
  ENDDO.

ENDFORM.
************************************************************************
************************************************************************
