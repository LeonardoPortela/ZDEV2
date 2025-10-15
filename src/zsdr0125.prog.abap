*&---------------------------------------------------------------------*
*& Report  ZSDR0125
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Job para Consulta de CCT via WS                         *
* Transação..: ZSDR0125                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zsdr0125.

************************************************************************
* tabelas
************************************************************************
TABLES: zsdt0264, j_1bnfdoc.

************************************************************************
* tela selecao
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_credat TYPE syst_datum DEFAULT sy-datum.
SELECT-OPTIONS: p_docnum FOR j_1bnfdoc-docnum NO INTERVALS.
SELECT-OPTIONS: p_chave FOR zsdt0264-chave NO INTERVALS.

PARAMETER:      p_dt AS CHECKBOX.   "DATA 30
PARAMETER:      p_dt_c AS CHECKBOX. "DATA DE CORTE
PARAMETER:      p_alv AS CHECKBOX.  "EXIBIR ALV

SELECTION-SCREEN END   OF BLOCK b1.

************************************************************************
* Types
************************************************************************
TYPES: BEGIN OF ty_nf,
         docnum TYPE docnum,
         chave  TYPE zsdt0264-chave.
TYPES: END   OF ty_nf.

TYPES: BEGIN OF ty_zlest0186,
         chave  TYPE zde_chave_doc_e,
         docnum TYPE j_1bnfe_active-docnum,
         docdat TYPE zsdt0264-docdat.
TYPES: END   OF ty_zlest0186.

TYPES: BEGIN OF ty_notas.
         INCLUDE STRUCTURE zsdt0264.
         TYPES: matnr TYPE j_1bnflin-matnr.
TYPES: END   OF ty_notas.

TYPES: BEGIN OF ty_nf_envio,
         docnum TYPE docnum,
         chave  TYPE zde_chave_doc_e,
         docdat TYPE zsdt0264-docdat.
TYPES: END   OF ty_nf_envio.

TYPES: BEGIN OF ty_uf,
         id TYPE n LENGTH 2,
         uf TYPE c LENGTH 2.
TYPES: END   OF ty_uf.

TYPES: BEGIN OF ty_chave,
         chave TYPE zde_chave_doc_e.
TYPES: END   OF ty_chave.

TYPES: BEGIN OF ty_geral,
         docnum   TYPE j_1bnfdoc-docnum,
         chave    TYPE   zsdt0264-chave,
         docdat   TYPE j_1bnfdoc-docdat,
         credat   TYPE j_1bnfdoc-credat,
         branch   TYPE j_1bnfdoc-branch,
         terminal TYPE lfa1-lifnr,
         lifnr    TYPE lfa1-lifnr,
         matnr    TYPE mara-matnr,
         regio    TYPE j_1bnfe_active-regio,
         nfyear   TYPE j_1bnfe_active-nfyear,
         nfmonth  TYPE j_1bnfe_active-nfmonth,
         stcd1    TYPE j_1bnfe_active-stcd1,
         model    TYPE j_1bnfe_active-model,
         serie    TYPE j_1bnfe_active-serie,
         nfnum9   TYPE j_1bnfe_active-nfnum9,
         docnum9  TYPE j_1bnfe_active-docnum9,
         cdv      TYPE j_1bnfe_active-cdv.
TYPES:     END   OF ty_geral.


************************************************************************
* VAriaveis
************************************************************************
DATA: t_notas              TYPE TABLE OF ty_notas,
      t_notas_aux          TYPE TABLE OF ty_notas,
      w_notas              TYPE ty_notas,
      t_nf                 TYPE TABLE OF ty_nf,
      w_nf                 TYPE ty_nf,
      t_nf_envio           TYPE TABLE OF ty_nf_envio,
      w_nf_envio           TYPE ty_nf_envio,
      t_nfe_cons           TYPE zde_chave_doc_e_t,
      w_nfe_cons           TYPE zde_chave_doc_e,
      t_nfe_fora           TYPE TABLE OF ty_chave,
      t_nfe_aux            TYPE TABLE OF ty_chave,
      w_nfe_aux            TYPE ty_chave,
      t_value              TYPE rgsb4 OCCURS 0 WITH HEADER LINE,
      t_zlest0186          TYPE TABLE OF ty_zlest0186,
      w_zlest0186          TYPE ty_zlest0186,
      w_zlest0146          TYPE zlest0146,
      w_zlest0147          TYPE zlest0147,
      t_zlest0168          TYPE zlest0168_t,
      t_retorno            TYPE zde_retorno_proc,
      l_credat             TYPE j_1bcredat,
      l_credat1            TYPE j_1bcredat,
      l_credat2            TYPE j_1bcredat,
      dt_corte             TYPE j_1bcredat,
      l_docdat             TYPE j_1bdocdat,
      l_erro               TYPE c,
      sem_dados            TYPE c,
      l_matnr              TYPE string,
      l_value(18)          TYPE n,
      it_uf                TYPE TABLE OF ty_uf,
      l_erro_autenticacao  TYPE char1,
      l_consulta_realizada TYPE char1,
      vl_matnr             TYPE char255,
      vl_werks             TYPE char255,
      v_cct_cp             TYPE c,
      it_geral             TYPE TABLE OF ty_geral.

RANGES:
      r_matnr               FOR mara-matnr,
      r_werks               FOR t001w-werks,
      r_bukrs               FOR t001-bukrs,
      r_lifnr               FOR lfa1-lifnr.

************************************************************************
* Initial
************************************************************************
INITIALIZATION.

  it_uf = VALUE #(
                  ( id = 11 uf = 'RO' ) ( id = 12 uf = 'AC' ) ( id = 13 uf = 'AM' ) ( id = 14 uf = 'RR' )
                  ( id = 15 uf = 'PA' ) ( id = 16 uf = 'AP' ) ( id = 17 uf = 'TO' ) ( id = 21 uf = 'MA' )
                  ( id = 22 uf = 'PI' ) ( id = 23 uf = 'CE' ) ( id = 24 uf = 'RN' ) ( id = 25 uf = 'PB' )
                  ( id = 26 uf = 'PE' ) ( id = 27 uf = 'AL' ) ( id = 28 uf = 'SE' ) ( id = 29 uf = 'BA' )
                  ( id = 31 uf = 'MG' ) ( id = 32 uf = 'ES' ) ( id = 33 uf = 'RJ' ) ( id = 35 uf = 'SP' )
                  ( id = 41 uf = 'PR' ) ( id = 42 uf = 'SC' ) ( id = 43 uf = 'RS' ) ( id = 50 uf = 'MS' )
                  ( id = 51 uf = 'MT' ) ( id = 52 uf = 'GO' ) ( id = 53 uf = 'DF' )
                 ).

************************************************************************
* START
************************************************************************
START-OF-SELECTION.

*"// Data de Corte onde Inicia o processo Automatico.
  dt_corte = '20210116'.

*-Verifica Job em execução
  PERFORM f_verifica_job CHANGING l_erro.

  CHECK l_erro = abap_false.

  IF p_dt IS INITIAL.
*-NFs 30 dias
    l_credat  = p_credat - 22. "// 22 dias de busca na standard
    l_credat1 = p_credat - 20. "// 20 dias de para enviar para o WS
    l_credat2 = l_credat - 8 . "// 2 + 8 dias de armazenamento
  ELSE.
    l_credat  = p_credat.
    l_credat1 = p_credat.
    l_credat2 = l_credat.
  ENDIF.

  IF p_alv EQ abap_false.

*-Selecao Notas
    PERFORM f_seleciona_nf.

*-Gravar tabela
    PERFORM f_grava_tabela.

* Selecao das NF gravadas para envio
    PERFORM f_selecao_envio.

*-Consulta portal
    PERFORM f_envia_portal.

* --Recepção de Notas
    PERFORM f_importa_recepcao.

* --Consuta 186
    PERFORM f_verifica_xcomex.

    MESSAGE s000(fb) WITH text-100.
  ELSE.

*-Selecao Notas
    PERFORM f_seleciona_nf.

    APPEND INITIAL LINE TO t_notas.
    CALL SCREEN 0100.

  ENDIF.


************************************************************************
* Job em execucao
************************************************************************
FORM f_verifica_job CHANGING p_erro.

  FREE: p_erro.

*-checa Job em execucao
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao(
            EXPORTING
              i_nome_program = sy-cprog
            IMPORTING
              e_qtd          = DATA(e_qtd) ).

      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd > 1.
      p_erro = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.

************************************************************************
* Selecao informacoes
************************************************************************
FORM f_seleciona_nf.

  FREE: t_nf, l_matnr, t_value, l_matnr, r_werks, r_matnr.

*-------------------------------------
* Busca de Docnum para processos manual
*-------------------------------------
  PERFORM f_get_docnum CHANGING sem_dados.

  IF p_chave IS NOT INITIAL.
    CHECK sem_dados IS NOT INITIAL.
  ENDIF.

*-------------------------------------
* Busca de Sets
*-------------------------------------
  PERFORM f_get_sets.

*-------------------------------------
* Notas cadastradas
*-------------------------------------
  SELECT docnum chave
    INTO TABLE t_nf
    FROM zsdt0264
    WHERE id_recepcao EQ '0000000000'.

*-------------------------------------
* Notas CCT
*-------------------------------------
  PERFORM f_consulta_geral.

ENDFORM.

************************************************************************
* GRavar tabela
************************************************************************
FORM f_grava_tabela.

  FREE: t_nfe_cons.

  LOOP AT t_notas INTO w_notas.

    IF w_notas-credat >= l_credat1.  "Enviar para consulta WS
      w_nfe_cons          = w_notas-chave.
      APPEND w_nfe_cons  TO t_nfe_cons.
    ENDIF.

  ENDLOOP.

ENDFORM.

************************************************************************
* Selecao NFs para envio
************************************************************************
FORM f_selecao_envio.

  CHECK p_docnum[] IS INITIAL.

  IF p_chave[] IS NOT INITIAL.
    CHECK sem_dados IS NOT INITIAL.
  ENDIF.

  FREE: t_nf_envio.

  SELECT docnum chave docdat
    FROM zsdt0264
    INTO TABLE t_nf_envio
   WHERE envdat >= l_credat2
     AND id_recepcao EQ '0000000000'.

  CHECK t_nf_envio[] IS NOT INITIAL.

  SORT t_nf_envio BY chave.
  DELETE ADJACENT DUPLICATES FROM t_nf_envio
                        COMPARING chave.

  LOOP AT t_nf_envio INTO w_nf_envio.
    w_nfe_cons          = w_nf_envio-chave.
    APPEND w_nfe_cons  TO t_nfe_cons.
  ENDLOOP.

ENDFORM.

************************************************************************
* Enviar portal
************************************************************************
FORM f_envia_portal.

  CHECK t_nfe_cons[] IS NOT INITIAL.

*-------------------------------------
* Consulta portal
*-------------------------------------
  CALL FUNCTION 'ZCCT_CONFIRMA_REC_NF_PORTAL'
    EXPORTING
      i_chaves             = t_nfe_cons[]
    IMPORTING
      e_erro_autenticacao  = l_erro_autenticacao
      e_consulta_realizada = l_consulta_realizada.

  LOOP AT t_nfe_cons INTO w_nfe_cons.
    w_nfe_aux-chave = w_nfe_cons.
    APPEND w_nfe_aux TO t_nfe_aux.

  ENDLOOP.

*-------------------------------------
* Chaves integradas
*-------------------------------------
  SELECT chave
    INTO TABLE t_zlest0186
    FROM zlest0186
     FOR ALL ENTRIES IN t_nfe_aux
   WHERE chave = t_nfe_aux-chave.

ENDFORM.

************************************************************************
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  F_RECEPCAO_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_importa_recepcao.

  SORT t_nf BY chave.

  CHECK t_zlest0186 IS NOT INITIAL.

  SELECT *
    FROM zlest0186
    INTO TABLE @DATA(it_zlest0186)
    FOR ALL ENTRIES IN @t_zlest0186
    WHERE chave EQ @t_zlest0186-chave.

  CHECK it_zlest0186 IS NOT INITIAL.

  LOOP AT t_zlest0186 ASSIGNING FIELD-SYMBOL(<f_zlest0186>).

    READ TABLE t_notas INTO w_notas WITH KEY  chave = <f_zlest0186>-chave.
    IF sy-subrc IS INITIAL.
      <f_zlest0186>-docnum = w_notas-docnum.
      <f_zlest0186>-docdat = w_notas-docdat.
    ELSE.
      READ TABLE t_nf_envio INTO w_nf_envio WITH KEY chave = <f_zlest0186>-chave.
      IF sy-subrc IS INITIAL.
        <f_zlest0186>-docnum = w_nf_envio-docnum.
        <f_zlest0186>-docdat = w_nf_envio-docdat.
      ENDIF.
    ENDIF.

    READ TABLE t_nf INTO w_nf WITH KEY chave = <f_zlest0186>-chave
                              BINARY SEARCH.
    CHECK sy-subrc IS NOT INITIAL.

    READ TABLE t_notas INTO w_notas WITH KEY chave = <f_zlest0186>-chave.

    CHECK w_notas-chave IS NOT INITIAL.

    CLEAR zsdt0264.
    zsdt0264-mandt      = sy-mandt.
    zsdt0264-docnum     = w_notas-docnum.
    zsdt0264-chave      = w_notas-chave.
    zsdt0264-docdat     = w_notas-docdat.
    zsdt0264-credat     = w_notas-credat.
    zsdt0264-envdat     = w_notas-credat.
    zsdt0264-branch     = w_notas-branch.
    zsdt0264-terminal   = w_notas-terminal.
    zsdt0264-dt_ent     = sy-datum.
    zsdt0264-hr_ent     = sy-uzeit.

    MODIFY zsdt0264.

  ENDLOOP.

  COMMIT WORK.

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE @DATA(it_active)
    FOR ALL ENTRIES IN @t_zlest0186
    WHERE docnum EQ @t_zlest0186-docnum.

  IF it_active[] IS NOT INITIAL.
    SELECT *
      FROM j_1bnfdoc
      INTO TABLE @DATA(it_doc)
      FOR ALL ENTRIES IN @it_active
      WHERE docnum EQ @it_active-docnum.

    SELECT *
      FROM j_1bnflin
      INTO TABLE @DATA(it_lin)
      FOR ALL ENTRIES IN @it_active
      WHERE docnum EQ @it_active-docnum.
  ENDIF.

  LOOP AT it_zlest0186 INTO DATA(wa_zlest0186).

    READ TABLE t_zlest0186 INTO DATA(lwa_nf_zlest0186) WITH KEY chave = wa_zlest0186-chave.
    CHECK sy-subrc eq 0.

    DATA(vl_docnum) = lwa_nf_zlest0186-docnum.

    IF wa_zlest0186-chave IS NOT INITIAL.

      SELECT SINGLE a~id_recepcao
        FROM zlest0146 AS a
        INTO @DATA(vl_id_recepcao)
       WHERE a~cancel EQ ''
         AND EXISTS ( SELECT *
                        FROM zlest0147 AS b
                       WHERE b~id_recepcao = a~id_recepcao
                         AND b~chave_nfe   = @wa_zlest0186-chave ).
      IF sy-subrc IS INITIAL.

        UPDATE zsdt0264 SET id_recepcao        = vl_id_recepcao
                            naoimportar        = abap_false
                            motivo             = 'Nota ja foi Recepcionada!'
         WHERE chave       EQ wa_zlest0186-chave
           and id_recepcao EQ '0000000000'.

        CONTINUE.
      ENDIF.
    ENDIF.

    w_zlest0146-cnpj_responsavel      = wa_zlest0186-cnpj_responsavel.
    w_zlest0146-local_codigo_urf      = wa_zlest0186-codigo_urf.
    w_zlest0146-local_codigo_ra       = wa_zlest0186-codigo_ra.

    IF w_zlest0146-local_codigo_ra IS INITIAL.

      SELECT COUNT(*)
        FROM setleaf
        WHERE setname EQ 'ZSDR125_RA_CNPJ'
          AND valfrom EQ w_zlest0146-cnpj_responsavel.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE lifnr
          FROM lfa1
          INTO @DATA(vl_lifnr)
         WHERE stcd1 EQ @w_zlest0146-cnpj_responsavel.

        IF sy-subrc IS INITIAL.

          SELECT SINGLE codigo_ra
            FROM zsdt0168
            INTO  w_zlest0146-local_codigo_ra
            WHERE lifnr = vl_lifnr.
          IF sy-subrc IS INITIAL.

            UPDATE zlest0186 SET codigo_ra = w_zlest0146-local_codigo_ra
            WHERE chave = wa_zlest0186-chave.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    w_zlest0146-local_latitude        = wa_zlest0186-latitude.
    w_zlest0146-local_longitude       = wa_zlest0186-longitude.
    w_zlest0146-transportador_cnpj    = wa_zlest0186-chave+6(14).
    w_zlest0147-chave_nfe             = wa_zlest0186-chave.
    w_zlest0147-nfnum9                = wa_zlest0186-chave+25(9).
    w_zlest0147-serie                 = wa_zlest0186-chave+22(3).
    w_zlest0147-model                 = wa_zlest0186-chave+20(2).
    w_zlest0147-nfyear                = wa_zlest0186-chave+2(2).
    w_zlest0147-nfmonth               = wa_zlest0186-chave+4(2).
    w_zlest0146-peso_aferido_recepcao = wa_zlest0186-peso_aferido.

    TRY .
        w_zlest0147-sigla_uf_emissor   = it_uf[ id = wa_zlest0186-chave+0(2) ]-uf.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


    w_zlest0147-emissor_cnpj = wa_zlest0186-chave+6(14).

    SELECT COUNT(*)
      FROM lfa1
     WHERE stcd1 EQ w_zlest0147-emissor_cnpj.

    IF sy-subrc IS NOT INITIAL.
      CLEAR w_zlest0147-emissor_cnpj.

      w_zlest0147-emissor_cpf = wa_zlest0186-chave+9(11).

    ENDIF.

*    TRY .
*        w_zlest0147-dt_emissao = it_doc[ docnum = vl_docnum ]-docdat.
*      CATCH cx_sy_itab_line_not_found.
*        CLEAR w_zlest0147-dt_emissao.
*    ENDTRY.

    w_zlest0147-dt_emissao = lwa_nf_zlest0186-docdat.

    DATA: lva_saldo_fiscal_portal TYPE j_1bnetqty, " VALUE '51.410',
          lva_saldo_fiscal_sap    TYPE j_1bnetqty, " VALUE '51410.000',
          lva_ncm_doc_fiscal      TYPE j1b_nf_xml_item-ncm.

    lva_saldo_fiscal_portal = wa_zlest0186-saldo.


    if vl_docnum is INITIAL. "Nota Fiscal de Produtor não escriturada no SAP
       w_zlest0146-dt_recepcao = wa_zlest0186-dt_recepcao.
    ELSE.

      lva_saldo_fiscal_sap    = it_lin[ docnum = vl_docnum ]-menge.
      lva_ncm_doc_fiscal      = it_lin[ docnum = vl_docnum ]-nbm.

      REPLACE ALL OCCURRENCES OF '.' IN lva_ncm_doc_fiscal WITH ''.
      CONDENSE lva_ncm_doc_fiscal NO-GAPS.

      IF w_zlest0146-peso_aferido_recepcao IS NOT INITIAL.

        SELECT SINGLE *
          FROM setleaf INTO @DATA(_wl_set_ncm_utrib)
         WHERE setname = 'MAGGI_NCM_UTRIB_EXP'
           AND valfrom = @lva_ncm_doc_fiscal.

        IF sy-subrc = 0. "Saldo fiscal do portal está na unidade tributavel em Tonelada
          lva_saldo_fiscal_portal = lva_saldo_fiscal_portal * 1000. "Converter para KG.
        ENDIF.

        IF lva_saldo_fiscal_portal EQ lva_saldo_fiscal_sap.
          w_zlest0146-dt_recepcao = wa_zlest0186-dt_recepcao.
        ELSE.
          UPDATE zsdt0264 SET naoimportar = abap_true
                              motivo      = 'Saldo fiscal já utilizado!'
                        WHERE docnum    = vl_docnum.
          CLEAR: w_zlest0146, w_zlest0147, t_zlest0168, t_retorno.
          CONTINUE.
        ENDIF.
      ELSE.
        w_zlest0146-dt_recepcao = wa_zlest0186-dt_recepcao.
      ENDIF.
    endif.

    CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
      EXPORTING
        i_docnum    = vl_docnum
        i_ck_cct_cp = abap_true
      IMPORTING
        e_cct_cp    = v_cct_cp.

    IF v_cct_cp IS NOT INITIAL.
      UPDATE zsdt0264 SET naoimportar = abap_true
                          motivo      = 'Já existe CCT para o documento!'
                      WHERE docnum    = vl_docnum.
      CLEAR: w_zlest0146, w_zlest0147, t_zlest0168, t_retorno.
      CONTINUE.
    ENDIF.

    IF ( w_zlest0146-peso_aferido_recepcao IS INITIAL ) AND ( lva_saldo_fiscal_sap > 0 ).

      SELECT COUNT(*)
        FROM setleaf
        WHERE setname EQ 'ZSDR125_PESO_AFE_CNPJ'
          AND valfrom EQ w_zlest0146-cnpj_responsavel.

      IF sy-subrc IS INITIAL.
        w_zlest0146-peso_aferido_recepcao = lva_saldo_fiscal_sap.

        UPDATE zlest0186 SET peso_aferido = lva_saldo_fiscal_sap
          WHERE chave = wa_zlest0186-chave.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ZCCT_PROC_RECEPCAO_CARGA'
      EXPORTING
        i_gravar_registro  = abap_true
        i_recinto_terceiro = abap_true
      CHANGING
        c_zlest0146        = w_zlest0146
        c_zlest0147        = w_zlest0147
        c_zlest0168        = t_zlest0168
        c_retorno          = t_retorno.

    CASE t_retorno-type.
      WHEN 'E'.
        DATA(msg) = |Erro na Importação: { t_retorno-texto }|.
        UPDATE zsdt0264 SET naoimportar = abap_true
                                 motivo = msg
                           WHERE docnum = vl_docnum.
        CLEAR: w_zlest0146, w_zlest0147, t_zlest0168, t_retorno.
        CONTINUE.

      WHEN 'W'.

        UPDATE zsdt0264 SET id_recepcao = t_retorno-texto+94(10)
                            naoimportar = abap_false
                            motivo = t_retorno-texto
                        WHERE docnum = vl_docnum.
        CLEAR: w_zlest0146, w_zlest0147, t_zlest0168, t_retorno.
        CONTINUE.

    ENDCASE.

    UPDATE zsdt0264 SET id_recepcao = w_zlest0146-id_recepcao
                        naoimportar = abap_false
                        motivo      = t_retorno-texto
                    WHERE docnum    = vl_docnum.

    CLEAR: w_zlest0146, w_zlest0147, t_zlest0168, t_retorno.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_GERAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_consulta_geral .

  TYPES: BEGIN OF ty_terminal,
           lifnr TYPE lfa1-lifnr.
  TYPES:     END   OF ty_terminal.

  DATA: it_sflight2 TYPE TABLE OF ty_nf.
  DATA: wa_sflight TYPE ty_nf.
  DATA: it_terminal TYPE TABLE OF ty_terminal.
  DATA: wa_terminal TYPE ty_terminal.

* "// Entrada Propria
  SELECT dc~docnum,
         ' ' AS chave,
         dc~docdat,
         dc~credat,
         dc~branch,
         ' ' AS terminal,
         ' ' AS lifnr,
         li~matnr,
         ac~regio, ac~nfyear, ac~nfmonth, ac~stcd1, ac~model, ac~serie, ac~nfnum9, ac~docnum9, ac~cdv
    FROM j_1bnfdoc AS dc
         INNER JOIN j_1bnfe_active AS ac ON dc~docnum = ac~docnum
         INNER JOIN j_1bnflin      AS li ON dc~docnum = li~docnum
    INTO TABLE @it_geral
   WHERE dc~credat  >= @l_credat
*         dc~credat  = @l_credat
     AND dc~docnum  IN @p_docnum
     AND dc~direct  = '1'  "Entrada
     AND dc~nfe     = 'X'  "Somente Eletronica
     AND dc~model   = '55' "Somente NFE
     AND dc~doctyp  <> '5' " não pode estar estornado
     AND dc~branch  IN @r_werks "somente essas filiais
     AND li~menge   > 0 "tem que ter saldo nessa nota
     AND li~matkl   NOT IN ('700140') "Algodão ñ entra
     AND ac~docsta  = '1' "Nota enviada
     AND ac~scssta  <> '2' "e Autorizada
     AND ac~cancel  = ' ' "ñ pode estar camcelada
     AND dc~entrad  = @abap_true. "Entrada propria

  PERFORM pf_get_nota_produtor.

* "// Entrada de Produtor
  SELECT dc~docnum,
         ' ' AS chave,
         dc~docdat,
         dc~credat,
         dc~branch,
         ' ' AS terminal,
         ' ' AS lifnr,
         li~matnr,
         ac~regio, ac~nfyear, ac~nfmonth, ac~stcd1, ac~model, ac~serie, ac~nfnum9, ac~docnum9, ac~cdv
    FROM j_1bnfdoc AS dc
         INNER JOIN j_1bnfe_active AS ac ON dc~docnum = ac~docnum
         INNER JOIN j_1bnflin      AS li ON dc~docnum = li~docnum
    INTO TABLE @it_geral
   WHERE dc~credat  >= @l_credat
*         dc~credat  = @l_credat
     AND dc~docnum  IN @p_docnum
     AND dc~direct  = '1'  "Entrada
     AND dc~nfe     = 'X'  "Somente Eletronica
     AND dc~model   = '55' "Somente NFE
     AND dc~doctyp  <> '5' " não pode estar estornado
     AND dc~branch  IN @r_werks "somente essas filiais
     AND li~menge   > 0 "tem que ter saldo nessa nota
     AND li~matkl   NOT IN ('700140') "Algodão ñ entra
     AND ac~docsta  = '1' "Nota enviada
     AND ac~scssta  <> '2' "e Autorizada
     AND ac~cancel  = ' ' "ñ pode estar camcelada
     AND NOT EXISTS ( SELECT docnum  " se existir dentro dessa tabela remove
                       FROM zsdt0264
                        WHERE id_recepcao <> '0000000000'
                          AND docnum      = ac~docnum ).


  LOOP AT it_geral INTO DATA(wa_docnum).
    wa_docnum-chave = |{ wa_docnum-regio }{ wa_docnum-nfyear }{ wa_docnum-nfmonth }{ wa_docnum-stcd1 }{ wa_docnum-model }{ wa_docnum-serie }{ wa_docnum-nfnum9 }{ wa_docnum-docnum9 }{ wa_docnum-cdv }|.
    APPEND CORRESPONDING #( wa_docnum ) TO t_notas.
  ENDLOOP.

  FREE it_geral.

  SELECT  dc~docnum,
          ' ' AS chave,
          dc~docdat,
          dc~credat,
          dc~branch,
          ' ' AS terminal,
          pa~lifnr,
          li~matnr,
          ac~regio, ac~nfyear, ac~nfmonth, ac~stcd1, ac~model, ac~serie, ac~nfnum9, ac~docnum9, ac~cdv
     FROM j_1bnfdoc AS dc
          INNER JOIN j_1bnfe_active AS ac ON dc~docnum = ac~docnum
          INNER JOIN j_1bnflin      AS li ON dc~docnum = li~docnum
          INNER JOIN vbrp           AS ft ON li~refkey = ft~vbeln "#EC CI_DB_OPERATION_OK[2768887]
          INNER JOIN likp           AS re ON ft~vgbel  = re~vbeln
          INNER JOIN vbpa           AS pa ON re~vbeln  = pa~vbeln
     INTO TABLE @it_geral
    WHERE dc~credat    >= @l_credat
*          dc~credat    = @l_credat
      AND dc~docnum    IN @p_docnum
      AND dc~direct    = '2'
      AND dc~nfe       = 'X'
      AND dc~model     = '55'
      AND pa~parvw     = 'Z1'
      AND ac~docsta    = '1'
      AND ac~scssta    <> '2'
      AND ac~cancel    = ' '
      AND li~menge     > 0
      AND li~reftyp    = 'BI'
      AND re~fkarv     = 'ZRFL'
      AND li~matkl     NOT IN ('700140')
      AND dc~bukrs     IN @r_bukrs
      and ft~draft     eq @space
      AND NOT EXISTS ( SELECT docnum
                        FROM zsdt0264
                         WHERE id_recepcao <> '0000000000'
                           AND docnum      = ac~docnum ).

*"//busca dados do Terminal
  PERFORM f_busca_terminal.

  LOOP AT it_geral INTO wa_docnum.
    APPEND CORRESPONDING #( wa_docnum ) TO t_notas.
  ENDLOOP.

  FREE it_geral.

  SELECT   d~docnum,
           ' ' AS chave,
           d~docdat,
           d~credat,
           d~branch,
           ' ' AS terminal,
           p~parid AS lifnr,
           l~matnr,
           a~regio, a~nfyear, a~nfmonth, a~stcd1, a~model, a~serie, a~nfnum9, a~docnum9, a~cdv
      FROM j_1bnfdoc AS d
           INNER JOIN j_1bnfe_active AS a ON d~docnum   = a~docnum
           INNER JOIN j_1bnflin      AS l ON d~docnum   = l~docnum
           INNER JOIN zfiwrt0008     AS z ON d~docnum   = z~docnum
           INNER JOIN zfiwrt0015     AS p ON z~seq_lcto = p~seq_lcto
    INTO TABLE @it_geral
    WHERE d~credat    >= @l_credat
*          d~credat    = @l_credat
      AND d~docnum   IN @p_docnum
      AND d~direct    = '2'
      AND d~nfe       = 'X'
      AND d~model     = '55'
      AND a~docsta    = '1'
      AND a~scssta    <> '2'
      AND a~cancel    = ' '
      AND l~menge     > 0
      AND l~reftyp    = 'ZW'
      AND d~bukrs     IN @r_bukrs
      AND z~ctrl_zrfl = 'S'
      AND p~parvw     = 'Z1'
      AND NOT EXISTS ( SELECT docnum
                        FROM zsdt0264
                         WHERE id_recepcao <> '0000000000'
                           AND docnum      = d~docnum ).

*"//busca dados do Terminal
  PERFORM f_busca_terminal.

  LOOP AT it_geral INTO wa_docnum.
    APPEND CORRESPONDING #( wa_docnum ) TO t_notas.
  ENDLOOP.

  IF p_dt_c IS INITIAL.
    DELETE t_notas WHERE credat < dt_corte.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_TERMINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_terminal.

  CHECK it_geral IS NOT INITIAL.

  TYPES: BEGIN OF ty_terminal,
           lifnr TYPE lfa1-lifnr.
  TYPES:     END   OF ty_terminal.

  DATA: it_terminal TYPE TABLE OF ty_terminal.
  DATA: wa_terminal TYPE ty_terminal.

  SELECT mandt, docnum, novo_terminal, doc_material, ano_material
    INTO TABLE @DATA(it_zcarta_correcao)
      FROM zcarta_correcao AS cc
    FOR ALL ENTRIES IN @it_geral
        WHERE cc~novo_terminal <> ' '
          AND cc~authcode      <> ' '
          AND cc~id_cc = ( SELECT MAX( id_cc )
                                FROM zcarta_correcao AS ccm
                                  WHERE ccm~docnum        = @it_geral-docnum
                                    AND ccm~novo_terminal <> ' '
                                    AND ccm~authcode      <> ' ').


  LOOP AT it_geral INTO DATA(wa_docnum).
    IF wa_docnum-lifnr IS NOT INITIAL.
      wa_terminal-lifnr = wa_docnum-lifnr.
      APPEND wa_terminal TO it_terminal.
    ENDIF.
  ENDLOOP.

  LOOP AT it_zcarta_correcao INTO DATA(wa_zcarta_correcao).
    IF wa_zcarta_correcao-novo_terminal IS NOT INITIAL.
      wa_terminal-lifnr = wa_zcarta_correcao-novo_terminal.
      APPEND wa_terminal TO it_terminal.
    ENDIF.
  ENDLOOP.

  IF it_terminal IS NOT INITIAL.
    SELECT *
      FROM lfa1 AS z1
      INTO TABLE @DATA(it_lfa1)
      FOR ALL ENTRIES IN @it_terminal
        WHERE lifnr EQ @it_terminal-lifnr.
  ENDIF.

  LOOP AT it_geral ASSIGNING FIELD-SYMBOL(<f_docnum>).

    <f_docnum>-chave = |{ <f_docnum>-regio }{ <f_docnum>-nfyear }{ <f_docnum>-nfmonth }{ <f_docnum>-stcd1 }{ <f_docnum>-model }{ <f_docnum>-serie }{ <f_docnum>-nfnum9 }{ <f_docnum>-docnum9 }{ <f_docnum>-cdv }|.
    READ TABLE it_zcarta_correcao INTO wa_zcarta_correcao WITH KEY docnum = <f_docnum>-docnum.
    IF sy-subrc IS INITIAL.
      <f_docnum>-terminal = wa_zcarta_correcao-novo_terminal.
    ELSE.
      <f_docnum>-terminal = <f_docnum>-lifnr.
    ENDIF.

    IF <f_docnum>-terminal IS NOT INITIAL.
      READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = <f_docnum>-terminal ktokk = 'ZFIC'.
      IF sy-subrc IS INITIAL.
        READ TABLE r_lifnr WITH KEY low = <f_docnum>-terminal TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          <f_docnum>-docnum = '0000000000'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  DELETE it_geral WHERE docnum = '0000000000'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SETS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_sets .

*-------------------------------------------------
* MAteriais
*-------------------------------------------------
  FREE t_value.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZSDR125_MATERIAIS'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_value.
    r_matnr-sign   = 'I'.
    r_matnr-option = 'EQ'.
    r_matnr-low    = t_value-from.
    APPEND r_matnr.
  ENDLOOP.

*-------------------------------------------------
* Filiais
*-------------------------------------------------
  FREE t_value.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZSDR125_FILIAIS'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_value.
    r_werks-sign   = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low    = t_value-from.
    APPEND r_werks.
  ENDLOOP.

*-------------------------------------------------
* Empresas
*-------------------------------------------------
  FREE t_value.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZSDR125_EMPRESAS'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_value.
    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low    = t_value-from.
    APPEND r_bukrs.
  ENDLOOP.


*-------------------------------------------------
* Exceção dos Portos ZFIC
*-------------------------------------------------
  FREE t_value.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZSDR125_EXCECAO_ZFIC'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_value.
    r_lifnr-sign   = 'I'.
    r_lifnr-option = 'EQ'.
    r_lifnr-low    = t_value-from.
    APPEND r_lifnr.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .

  DATA: str      TYPE REF TO data,
        obj_cont TYPE REF TO cl_gui_custom_container,
        obj_alv  TYPE REF TO cl_gui_alv_grid,
        it_fcat  TYPE lvc_t_fcat.

  ASSIGN 'TY_NOTAS' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA str TYPE (<fs_str>).

  it_fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<f_cat>).
    IF <f_cat>-fieldname = 'MOTIVO'.
      <f_cat>-outputlen = '10'.
    ENDIF.
  ENDLOOP.

  BREAK-POINT.

  IF obj_cont IS INITIAL.

    CREATE OBJECT obj_cont
      EXPORTING
        container_name = 'CC'.

    CREATE OBJECT obj_alv
      EXPORTING
        i_shellstyle    = 0
        i_parent        = obj_cont
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    CALL METHOD obj_alv->set_table_for_first_display
      CHANGING
        it_outtab                     = t_notas[]
        it_fieldcatalog               = it_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  PERFORM f_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DOCNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_docnum CHANGING sem_dados.

  CHECK p_chave IS NOT INITIAL.
  FREE p_docnum.

  LOOP AT p_chave INTO DATA(w_chave).

    SELECT SINGLE docnum
      FROM j_1bnfe_active
      INTO @DATA(vl_docnum)
  WHERE regio   = @w_chave-low(2)
    AND nfyear  = @w_chave-low+2(2)
    AND nfmonth = @w_chave-low+4(2)
    AND stcd1   = @w_chave-low+6(14)
    AND model   = @w_chave-low+20(2)
    AND serie   = @w_chave-low+22(3)
    AND nfnum9  = @w_chave-low+25(9)
    AND docnum9 = @w_chave-low+34(9)
    AND cdv     = @w_chave-low+43(1).

    CHECK sy-subrc IS INITIAL.

    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = vl_docnum
                  ) TO p_docnum.

  ENDLOOP.

  CHECK p_docnum[] IS NOT INITIAL.

  l_credat = '20000318'.  "Pega todas as notas desde do ano de 2000 com o Range de DOcnum encontrado no select de cima
  p_dt_c = abap_true.     "não verifica a data de Corte para importação

  l_credat1 = l_credat.
  sem_dados = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_XCOMEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_verifica_xcomex .

  SELECT *
    INTO TABLE @DATA(it_delete)
    FROM zsdt0264 AS A
    WHERE id_recepcao EQ '0000000000'
      AND NOT EXISTS ( SELECT B~CHAVE
                         FROM zlest0186 AS B
                        WHERE B~chave EQ A~chave ).

  LOOP AT it_delete INTO DATA(wa_delete).

    SELECT COUNT(*)
      FROM zlest0186
      WHERE chave EQ wa_delete-chave.

    IF sy-subrc IS NOT INITIAL.
      DELETE FROM zsdt0264 WHERE chave EQ wa_delete-chave.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_GET_NOTA_PRODUTOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_get_nota_produtor .

  DATA: w_notas TYPE ty_notas.

  CHECK it_geral IS NOT INITIAL.

  SELECT *
    FROM zmmt_ee_zgr_docs
    INTO TABLE @DATA(it_zmmt_ee_zgr_docs)
    FOR ALL ENTRIES IN @it_geral
  WHERE docnum EQ @it_geral-docnum.

  CHECK it_zmmt_ee_zgr_docs IS NOT INITIAL.

  SELECT *
    FROM zmmt_ee_zgr
    INTO TABLE @DATA(it_zmmt_ee_zgr)
    FOR ALL ENTRIES IN @it_zmmt_ee_zgr_docs
    WHERE obj_key EQ @it_zmmt_ee_zgr_docs-obj_key.

  CHECK it_zmmt_ee_zgr IS NOT INITIAL.

  SELECT *
    FROM zsdt0001
    INTO TABLE @DATA(it_zsdt0001)
    FOR ALL ENTRIES IN @it_zmmt_ee_zgr
    WHERE ch_referencia EQ @it_zmmt_ee_zgr-ch_referencia.

  LOOP AT it_zsdt0001 INTO DATA(wa_zsdt0001).

    IF  wa_zsdt0001-chave_nfe IS NOT INITIAL.
      CLEAR w_notas.
      w_notas-chave  = wa_zsdt0001-chave_nfe.
      w_notas-credat = wa_zsdt0001-docdat.
      w_notas-docdat = wa_zsdt0001-docdat.
      APPEND w_notas TO t_notas.
    ELSE.

      SELECT SINGLE *
        FROM lfa1
        INTO @DATA(wa_lfa1)
        WHERE lifnr EQ @wa_zsdt0001-parid.

      IF sy-subrc IS INITIAL.

        IF wa_lfa1-stcd2 IS NOT INITIAL.

          SELECT SINGLE *
            FROM zib_nfe_dist_ter
            INTO @DATA(wa_zib_nfe_dist_ter)
            WHERE forne_cpf  = @wa_lfa1-stcd2
            AND numero = @wa_zsdt0001-nfnum.

          IF sy-subrc IS INITIAL.
            w_notas-chave  = wa_zib_nfe_dist_ter-chave_nfe.
            w_notas-credat = wa_zib_nfe_dist_ter-dt_emissao.
            w_notas-docdat = wa_zib_nfe_dist_ter-dt_emissao.
            APPEND w_notas TO t_notas.
          ENDIF.

        ELSEIF wa_lfa1-stcd1 IS NOT INITIAL.

          SELECT SINGLE *
            FROM zib_nfe_dist_ter
            INTO wa_zib_nfe_dist_ter
            WHERE forne_cnpj  = wa_lfa1-stcd1
            AND numero = wa_zsdt0001-nfnum.

          IF sy-subrc IS INITIAL.
            w_notas-chave  = wa_zib_nfe_dist_ter-chave_nfe.
            w_notas-credat = wa_zib_nfe_dist_ter-dt_emissao.
            w_notas-docdat = wa_zib_nfe_dist_ter-dt_emissao.
            APPEND w_notas TO t_notas.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    CLEAR: wa_zib_nfe_dist_ter, w_notas, wa_lfa1.
    FREE: it_zmmt_ee_zgr_docs, it_zmmt_ee_zgr.
  ENDLOOP.

  FREE:it_zsdt0001.

ENDFORM.
