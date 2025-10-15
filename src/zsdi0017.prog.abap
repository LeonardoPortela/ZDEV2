*&---------------------------------------------------------------------*
*& Report  ZSDI0017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsdi0017.

"--------------------
TYPES: BEGIN OF ty_zlest0087.
         INCLUDE STRUCTURE zlest0087.
TYPES:  del(1),
       END OF ty_zlest0087.

TYPES:
  BEGIN OF ty_zsdt0001,
    doc_rem       TYPE zsdt0001-doc_rem,
    branch        TYPE zsdt0001-branch,
    ch_referencia TYPE zsdt0001-ch_referencia,
    id_referencia TYPE zsdt0001-id_referencia,
    nr_safra      TYPE zsdt0001-nr_safra,
    bukrs         TYPE zsdt0001-bukrs,
  END   OF ty_zsdt0001,

  BEGIN OF ty_likp,
    vbeln TYPE likp-vbeln,
    xblnr TYPE likp-xblnr,
  END   OF ty_likp,


  BEGIN OF ty_zsdt0001_aux,
    nr_romaneio TYPE zsdt0001-nr_romaneio,
    nr_safra    TYPE zsdt0001-nr_safra,
    bukrs       TYPE zsdt0001-bukrs,
    branch      TYPE zsdt0001-branch,
  END   OF ty_zsdt0001_aux,

  BEGIN OF ty_zsdt0001_terc,
    ch_referencia    TYPE zsdt0001-ch_referencia,
    nr_nota_conta_or TYPE zsdt0001-nr_nota_conta_or,
    nr_serie_conta_o TYPE zsdt0001-nr_serie_conta_o,
    nfnum            TYPE zsdt0001-nfnum,
    series           TYPE zsdt0001-series,
    parid            TYPE zsdt0001-parid,

    nr_romaneio      TYPE zsdt0001-nr_romaneio,
    nr_safra         TYPE zsdt0001-nr_safra,
    bukrs            TYPE zsdt0001-bukrs,
    branch           TYPE zsdt0001-branch,
  END   OF ty_zsdt0001_terc,

  BEGIN OF ty_vbfa_aux,
    vbelv TYPE vbfa-vbelv,
    vbeln TYPE vbfa-vbeln,
  END   OF ty_vbfa_aux,

  BEGIN OF ty_vbfa,
    vbelv  TYPE vbfa-vbelv,
    refkey TYPE j_1bnflin-refkey,
  END   OF ty_vbfa,

  BEGIN OF ty_j_1bnflin,
    docnum TYPE j_1bnflin-docnum,
    refkey TYPE j_1bnflin-refkey,
    menge  TYPE j_1bnflin-menge,
    matnr  TYPE j_1bnflin-matnr,
  END   OF ty_j_1bnflin,

  BEGIN OF ty_j_1bnfdoc,
    docnum TYPE j_1bnfdoc-docnum,
    nfenum TYPE j_1bnfdoc-nfenum,
    series TYPE j_1bnfdoc-series,
    pstdat TYPE j_1bnfdoc-pstdat,
  END   OF ty_j_1bnfdoc,

  BEGIN OF ty_j_1bnfe_active,
    docnum TYPE j_1bnfe_active-docnum,
    docsta TYPE j_1bnfe_active-docsta,
    scssta TYPE j_1bnfe_active-scssta,
  END   OF ty_j_1bnfe_active,

  BEGIN OF y_branch,
    bukrs  TYPE j_1bbranch-bukrs,
    branch TYPE j_1bbranch-branch,
    stcd1  TYPE j_1bbranch-stcd1,
  END OF y_branch.
"-----------------------




DATA: ti_zsdt0001       TYPE TABLE OF ty_zsdt0001,
      ti_likp           TYPE TABLE OF ty_likp,
      ti_zsdt0001_aux   TYPE TABLE OF ty_zsdt0001_aux,
      ti_zsdt0001_terc  TYPE TABLE OF  ty_zsdt0001_terc,
      ti_vbfa           TYPE TABLE OF ty_vbfa,
      ti_vbfa_aux       TYPE TABLE OF ty_vbfa_aux,
      ti_j_1bnflin      TYPE TABLE OF ty_j_1bnflin,
      ti_j_1bnfe_active TYPE TABLE OF ty_j_1bnfe_active,
      ti_j_1bnfdoc      TYPE TABLE OF ty_j_1bnfdoc,
      ti_setleaf        LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
      ti_centros        TYPE TABLE OF lxhme_range_c10,
      ti_zlest0041      TYPE TABLE OF zlest0041,
      t_zlest0019       TYPE TABLE OF zlest0019,
      t_zlest0035       TYPE TABLE OF zlest0035,

      t_branch_aux      TYPE TABLE OF y_branch,
      ti_zlest0087      TYPE TABLE OF ty_zlest0087,
      ti_zlest0087_10   TYPE TABLE OF ty_zlest0087,
      ti_zlest0088      TYPE TABLE OF zlest0088,
      ti_zlest0088_10   TYPE TABLE OF zlest0088,
      ti_zlest0088_20   TYPE TABLE OF zlest0088.

DATA: wa_zsdt0001       TYPE ty_zsdt0001,
      wa_zsdt0001_aux   TYPE ty_zsdt0001_aux,
      wa_zsdt0001_terc  TYPE ty_zsdt0001_terc,
      wa_vbfa           TYPE ty_vbfa,
      wa_vbfa_aux       TYPE ty_vbfa_aux,
      wa_j_1bnflin      TYPE ty_j_1bnflin,
      wa_j_1bnfe_active TYPE  ty_j_1bnfe_active,
      wa_j_1bnfdoc      TYPE ty_j_1bnfdoc,
      wa_branch_aux     TYPE y_branch,
      wa_zlest0041      TYPE zlest0041,
      st_zlest0019      TYPE zlest0019,
      sl_zlest0035      TYPE zlest0035,
      wa_zlest0087      TYPE ty_zlest0087,
      wa_zlest0088_2    TYPE zlest0088,
      wa_zlest0088      TYPE zlest0088,
      wa_zlest0089      TYPE zlest0089,
      wa_zlest0087_10   TYPE ty_zlest0087,
      wa_zlest0088_10   TYPE zlest0088,
      wa_zlest0088_20   TYPE zlest0088,
      wa_setleaf        TYPE setleaf,
      wa_centros        TYPE lxhme_range_c10.

DATA: vl_name1     TYPE lfa1-name1,
      vl_stcd1     TYPE stcd1,
      vl_stcd1_2   TYPE stcd1,
      vl_stcd2     TYPE stcd2,
      vl_cnpjferro TYPE zlest0019-cnpjferro,
      vl_id_refkey TYPE zlest0019-id_zlest0019,
      vl_nfenum    TYPE j_1bnfdoc-nfenum,
      vl_nfnum     TYPE j_1bnfdoc-nfnum,
      vl_nfnuma    TYPE j_1bnfdoc-nfnum,
      vl_nfenuma   TYPE j_1bnfdoc-nfenum,
      vl_docnum    TYPE j_1bnfdoc-docnum,
      vl_nfnum_a   TYPE char6,
      vl_saida     TYPE char9,
      tabix        TYPE sy-tabix.
*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: c_x            TYPE c VALUE 'X',
           c_log(10)      TYPE c VALUE 'LOG',
           c_proc(10)     TYPE c VALUE 'PROC',
           c_ent(10)      TYPE c VALUE 'ENT',
           c_l1(2)        TYPE c VALUE 'L1',      " Confirmação de saída Ferroviário
           c_asc(10)      TYPE c VALUE 'ASC',
           c_mask_loc(6)  TYPE c VALUE '*.*',
           c_mask_unix(6) TYPE c VALUE '*.*',
           c_u            TYPE c VALUE 'U',
           c_w            TYPE c VALUE 'W',
           c_l            TYPE c VALUE 'L',
           c_e            TYPE c VALUE 'E',
           c_s            TYPE c VALUE 'S'.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*

"Para Execução em backgound (jobs) """"""""""""""""""""""""""""
IF sy-batch EQ abap_true.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
      e_qtd = 1.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.



"START-OF-SELECTION.

PERFORM : f_check_depara_nf_cancel. "Checar se possui depara com NF propria cancelada

PERFORM : f_seleciona_dados,
          f_processa_dados,
          f_processa_terceiro_l1,
          f_processa_terceiro_l2,
          f_processa_terceiro_l3.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: lv_data TYPE sy-datum.

  REFRESH: ti_zsdt0001,
           ti_vbfa     ,
           ti_j_1bnflin,
           ti_j_1bnfdoc,
           ti_centros  ,
           ti_vbfa_aux .

  "--------------Inicio set

  SELECT * INTO TABLE ti_setleaf
    FROM setleaf
   WHERE setname EQ 'CENTRO_NF_CONTA_ORDEM'.

  LOOP AT ti_setleaf INTO wa_setleaf.
    wa_centros-sign   = 'I'.
    wa_centros-option = 'EQ'.
    wa_centros-low    = wa_setleaf-valfrom.
    wa_centros-high   = wa_setleaf-valfrom.
    APPEND wa_centros TO ti_centros.
  ENDLOOP.

  "--------------Fim set

  lv_data = sy-datum - 90.

  SELECT a~doc_rem
         a~branch
         a~ch_referencia
         a~id_referencia
         a~nr_safra
         a~bukrs
    INTO TABLE ti_zsdt0001
    FROM zsdt0001 AS a
   WHERE a~branch IN ti_centros
     AND a~id_referencia NE ''
     AND a~doc_rem       NE ''
     AND a~dt_movimento GE lv_data
     AND  NOT EXISTS ( SELECT b~ch_referencia
                        FROM zlest0041 AS b
                        WHERE b~ch_referencia = a~ch_referencia ).

  CHECK ti_zsdt0001[] IS NOT INITIAL.

  LOOP AT ti_zsdt0001 INTO wa_zsdt0001 .

*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = wa_zsdt0001-id_referencia
*          IMPORTING
*            output = wa_zsdt0001_aux-nr_romaneio.

    wa_zsdt0001_aux-nr_romaneio = | { wa_zsdt0001-id_referencia ALPHA = IN }|.
    wa_zsdt0001_aux-nr_safra    = wa_zsdt0001-nr_safra .
    wa_zsdt0001_aux-bukrs       = wa_zsdt0001-bukrs    .
    wa_zsdt0001_aux-branch      = wa_zsdt0001-branch   .

    APPEND wa_zsdt0001_aux TO ti_zsdt0001_aux.

  ENDLOOP.

  SELECT ch_referencia
         nr_nota_conta_or
         nr_serie_conta_o
         nfnum
         series
         parid
         nr_romaneio
         nr_safra
         bukrs
         branch
    FROM zsdt0001
    INTO TABLE ti_zsdt0001_terc
     FOR ALL ENTRIES IN ti_zsdt0001_aux
   WHERE nr_romaneio = ti_zsdt0001_aux-nr_romaneio
     AND nr_safra    = ti_zsdt0001_aux-nr_safra
     AND bukrs       = ti_zsdt0001_aux-bukrs
     AND branch      = ti_zsdt0001_aux-branch
     AND ( nr_nota_conta_or NE '' OR nfnum NE '' ).

  CHECK ti_zsdt0001_terc[] IS NOT INITIAL.

  SELECT vbelv
         vbeln
    FROM vbfa
    INTO TABLE ti_vbfa_aux
    FOR ALL ENTRIES IN ti_zsdt0001
   WHERE vbelv   EQ ti_zsdt0001-doc_rem
     AND vbtyp_n EQ 'M'
     AND vbtyp_v EQ 'J'.

  CHECK ti_vbfa_aux[] IS NOT INITIAL.

  SELECT vbeln xblnr
    FROM likp INTO TABLE ti_likp
    FOR ALL ENTRIES IN ti_zsdt0001
   WHERE vbeln   EQ ti_zsdt0001-doc_rem.

  CHECK ti_likp[] IS NOT INITIAL.

  LOOP AT ti_vbfa_aux INTO wa_vbfa_aux.

    CLEAR wa_vbfa.

    wa_vbfa-vbelv  = wa_vbfa_aux-vbelv.
    wa_vbfa-refkey = wa_vbfa_aux-vbeln.

    APPEND wa_vbfa TO ti_vbfa.

  ENDLOOP.

  SELECT docnum
         refkey
         menge
         matnr
    FROM j_1bnflin
    INTO TABLE ti_j_1bnflin
     FOR ALL ENTRIES IN ti_vbfa
   WHERE refkey EQ ti_vbfa-refkey.

  CHECK ti_j_1bnflin[] IS NOT INITIAL.

  SELECT docnum
         nfenum
         series
         pstdat
    FROM j_1bnfdoc
    INTO TABLE ti_j_1bnfdoc
     FOR ALL ENTRIES IN ti_j_1bnflin
   WHERE docnum EQ ti_j_1bnflin-docnum .

  CHECK ti_j_1bnfdoc[] IS NOT INITIAL.

  SELECT docnum docsta scssta INTO TABLE ti_j_1bnfe_active
    FROM j_1bnfe_active
    FOR ALL ENTRIES IN ti_j_1bnfdoc
    WHERE docnum = ti_j_1bnfdoc-docnum.

  LOOP AT ti_j_1bnfe_active INTO wa_j_1bnfe_active.
    IF NOT ( wa_j_1bnfe_active-docsta = 1 AND wa_j_1bnfe_active-scssta <> '2' ).
      DELETE ti_j_1bnfdoc WHERE docnum = wa_j_1bnfe_active-docnum.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_dados .

  SORT : ti_zsdt0001      BY doc_rem,
         ti_zsdt0001_terc BY nr_romaneio nr_safra bukrs branch,
         ti_vbfa          BY vbelv,
         ti_j_1bnflin     BY refkey,
         ti_j_1bnfdoc     BY docnum,
         ti_likp          BY vbeln.

  DATA: vl_nr_romaneio TYPE zsdt0001-nr_romaneio.

  LOOP AT ti_zsdt0001 INTO wa_zsdt0001.

    READ TABLE ti_likp INTO DATA(lwa_likp) WITH KEY vbeln = wa_zsdt0001-doc_rem BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    CHECK lwa_likp-xblnr = wa_zsdt0001-ch_referencia. "Validar remessa que esta no romaneio.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zsdt0001-id_referencia
      IMPORTING
        output = vl_nr_romaneio.

    READ TABLE ti_zsdt0001_terc INTO wa_zsdt0001_terc WITH KEY nr_romaneio = vl_nr_romaneio
                                                               nr_safra    = wa_zsdt0001-nr_safra
                                                               bukrs       = wa_zsdt0001-bukrs
                                                               branch      = wa_zsdt0001-branch BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE ti_vbfa INTO wa_vbfa WITH KEY vbelv = wa_zsdt0001-doc_rem BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE ti_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_vbfa-refkey BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE ti_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    wa_zlest0041-centro_comprador = wa_zsdt0001-branch.

    IF NOT wa_zsdt0001_terc-nr_nota_conta_or IS INITIAL .
      wa_zlest0041-nr_nf       = wa_zsdt0001_terc-nr_nota_conta_or.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zsdt0001_terc-nr_serie_conta_o
        IMPORTING
          output = wa_zlest0041-serie.

    ELSE.
      wa_zlest0041-nr_nf       = wa_zsdt0001_terc-nfnum .

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zsdt0001_terc-series
        IMPORTING
          output = wa_zlest0041-serie.

    ENDIF.

    wa_zlest0041-cod_cliente   = wa_zsdt0001_terc-parid.
    wa_zlest0041-ch_referencia = wa_zsdt0001-ch_referencia.
    wa_zlest0041-nr_nf_propria = wa_j_1bnfdoc-nfenum.
    wa_zlest0041-serie_propria = wa_j_1bnfdoc-series.
    wa_zlest0041-docnum        = wa_j_1bnfdoc-docnum.
    wa_zlest0041-data_emissao  = wa_j_1bnfdoc-pstdat.
    wa_zlest0041-quantidade    = wa_j_1bnflin-menge .
    wa_zlest0041-cod_material  = wa_j_1bnflin-matnr .
    wa_zlest0041-usuario       = 'R3JOB'.
    wa_zlest0041-data          = sy-datum.
    wa_zlest0041-hora          = sy-uzeit.

    IF wa_zlest0041-docnum        IS NOT INITIAL AND wa_zlest0041-nr_nf_propria IS NOT INITIAL AND
       wa_zlest0041-cod_cliente   IS NOT INITIAL AND wa_zlest0041-serie         IS NOT INITIAL AND
       wa_zlest0041-nr_nf_propria IS NOT INITIAL AND wa_zlest0041-serie_propria IS NOT INITIAL AND
       wa_zlest0041-docnum        IS NOT INITIAL AND wa_zlest0041-data_emissao  IS NOT INITIAL AND
       wa_zlest0041-nr_nf         IS NOT INITIAL AND wa_zlest0041-nr_nf NE '000000000'.

      APPEND wa_zlest0041 TO ti_zlest0041.
    ENDIF.

    CLEAR : wa_zsdt0001_terc, wa_vbfa, wa_j_1bnflin, wa_j_1bnfdoc, wa_zlest0041.

  ENDLOOP.

  MODIFY zlest0041 FROM TABLE ti_zlest0041.


ENDFORM.                    " F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_TERCEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_terceiro_l1 .

  DATA: flag(1).

  SELECT *
    FROM zlest0087
    INTO TABLE ti_zlest0087_10
    WHERE tp_reg = '10'
    AND   status = ''.

  CHECK ti_zlest0087_10[] IS NOT INITIAL.

  SELECT *
  FROM zlest0087
  INTO TABLE ti_zlest0087
  FOR ALL ENTRIES IN ti_zlest0087_10
  WHERE id_refkey = ti_zlest0087_10-id_refkey
  AND   tp_reg = '30'
  AND   status = ''.

  REFRESH t_zlest0019.
  SORT: ti_zlest0087_10 BY id_refkey,
        ti_zlest0087    BY id_refkey.

  LOOP AT ti_zlest0087_10 INTO wa_zlest0087_10.
    CLEAR flag.
    LOOP AT ti_zlest0087 INTO wa_zlest0087 WHERE id_refkey = wa_zlest0087_10-id_refkey.
      "REG30
      tabix = sy-tabix.
      "
      SELECT SINGLE *
       INTO wa_zlest0041
       FROM zlest0041
      WHERE nr_nf       = wa_zlest0087-nr_nf
        AND cod_cliente = wa_zlest0087-lifnr.

      IF sy-subrc = 0.
*   Documento de nota fiscal eletrônica
        IF wa_zlest0041-docnum IS INITIAL.
          SELECT SINGLE docnum nfenum
            INTO (vl_docnum, vl_nfenum)
            FROM j_1bnfdoc
          WHERE nfenum = wa_zlest0087-nr_nf
            AND bukrs  = wa_zlest0087-bukrs
            AND branch = wa_zlest0087-werks
            AND direct = 2
            AND  cancel <> c_x.
        ELSE.
          vl_docnum = wa_zlest0041-docnum.
          vl_nfenum = wa_zlest0041-nr_nf_propria.
        ENDIF.

        CLEAR st_zlest0019.

        CLEAR: vl_name1, vl_stcd1, vl_stcd2.
        SELECT SINGLE name1 stcd1 stcd2
          INTO (vl_name1, vl_stcd1, vl_stcd2)
          FROM lfa1
         WHERE lifnr = wa_zlest0087-lifnr.

        IF vl_stcd1 IS INITIAL.
          vl_stcd1 = vl_stcd2.
        ENDIF.

        CLEAR vl_nfnum.

        CLEAR wa_zlest0089.
        SELECT SINGLE *
         FROM zlest0089
         INTO wa_zlest0089
         WHERE matnr  = wa_zlest0087-produto
         AND   bukrs  = wa_zlest0087-bukrs.

        PERFORM zgeranumero(zlesi0005) CHANGING st_zlest0019-id_zlest0019.

        MOVE:
        sy-mandt                    TO st_zlest0019-mandt,
        c_l1                        TO st_zlest0019-idinter,
        c_e                         TO st_zlest0019-tp_movi,
        wa_zlest0087-tp_reg         TO st_zlest0019-tp_reg,
        vl_stcd1                    TO st_zlest0019-cnpjcliente,
        wa_zlest0087-bukrs          TO st_zlest0019-bukrs,
        wa_zlest0087-werks          TO st_zlest0019-branch,
        vl_nfenum                   TO st_zlest0019-nfenum,
        vl_nfnum                    TO st_zlest0019-nfnum,
        wa_zlest0087-peso_nf        TO st_zlest0019-pesonf,
        wa_zlest0087-descarga_rodo  TO st_zlest0019-pesodvagao,
        wa_zlest0087-dt_descarga    TO st_zlest0019-dtachegada,
        wa_zlest0089-produto        TO st_zlest0019-produto,
        sy-datum                    TO st_zlest0019-erdat,
        sy-uzeit                    TO st_zlest0019-erzet,
        sy-uname                    TO st_zlest0019-uname,
        wa_zlest0041-nr_nf          TO st_zlest0019-nr_nf_terceiro,
        wa_zlest0041-cod_cliente    TO st_zlest0019-cod_fornecedor,
        wa_zlest0087-id_refkey      TO st_zlest0019-id_refkey.
        "WA_ZLEST0087-CHAVE          TO ST_ZLEST0019-CHAVE.

*        CLEAR: VL_NAME1, VL_STCD1, VL_STCD2.
*        SELECT SINGLE NAME1 STCD1 STCD2
*         INTO (VL_NAME1, VL_STCD1, VL_STCD2)
*         FROM LFA1
*        WHERE STCD1 = WA_ZLEST0087_10-CNPJ_PREST_SERV.
*        IF NOT VL_STCD1 IS INITIAL.
*          MOVE VL_STCD1             TO ST_ZLEST0019-CNPJFERRO.
*        ELSEIF NOT VL_STCD2 IS INITIAL.
*          MOVE VL_STCD2             TO ST_ZLEST0019-CNPJFERRO.
*        ELSE.
*          MOVE WA_ZLEST0087_10-EMPRESA   TO ST_ZLEST0019-CNPJFERRO.
*        ENDIF.

        CONCATENATE wa_zlest0087-bukrs '-' wa_zlest0087-werks '-' vl_nfenum vl_nfnum INTO st_zlest0019-chave SEPARATED BY space.
        CONDENSE st_zlest0019-chave NO-GAPS.

        SELECT SINGLE * INTO @DATA(wa_zlest0039)
          FROM zlest0039
         WHERE nfenum EQ @st_zlest0019-nfenum
           AND bukrs  EQ @st_zlest0019-bukrs
           AND werks  EQ @st_zlest0019-branch.

        IF sy-subrc IS INITIAL.
          DATA(lw_zlest0039_ajuste) = wa_zlest0039.
          lw_zlest0039_ajuste-datatransb  = st_zlest0019-dtachegada.
          lw_zlest0039_ajuste-pesotransb  = st_zlest0019-pesodvagao.
          CLEAR: lw_zlest0039_ajuste-datachegada, lw_zlest0039_ajuste-pesochegada.
          PERFORM verificar_carguero IN PROGRAM zlesi0005 USING wa_zlest0039 lw_zlest0039_ajuste IF FOUND.
          CLEAR: lw_zlest0039_ajuste.
        ENDIF.

        "L1
        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = wa_zlest0039-docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          UPDATE zlest0039 SET datatransb     = st_zlest0019-dtachegada
                              pesotransb      = st_zlest0019-pesodvagao
                              unidadetransb   = zlest0039~unidadesaida
                              "L2
                              dataterminal    = ''
                              pesoterminal    = ''
                              unidadeterminal = ''
                              "L3
                              datachegada     = ''
                              pesochegada     = ''
                              unidadechegada  = ''
                              status          = 'L1'
         WHERE nfenum EQ st_zlest0019-nfenum
         AND   bukrs  EQ st_zlest0019-bukrs
         AND   werks  EQ st_zlest0019-branch.
          CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum = wa_zlest0039-docnum.

        ENDIF.
*        UPDATE ZLEST0039 SET DATATRANSB      = ''
*                             PESOTRANSB      = ''
*                             UNIDADETRANSB   = ''
*                             "L2
*                             DATATERMINAL    = ''
*                             PESOTERMINAL    = ''
*                             UNIDADETERMINAL = ''
*                             "L3
*                             DATACHEGADA     = ''
*                             PESOCHEGADA     = ''
*                             UNIDADECHEGADA  = ''
*                             STATUS          = 'ET'
*        WHERE NFENUM EQ ST_ZLEST0019-NFENUM
*          AND CNPJ EQ ST_ZLEST0019-CNPJCLIENTE.

        "07/07/14 ALRS
        CLEAR: vl_name1, vl_stcd1, vl_stcd2.
        SELECT SINGLE name1 stcd1 stcd2
              INTO (vl_name1, vl_stcd1, vl_stcd2)
              FROM lfa1
             WHERE stcd1 = wa_zlest0087_10-cnpj_prest_serv.

        IF NOT vl_stcd1 IS INITIAL.
          MOVE vl_stcd1             TO st_zlest0019-cnpjferro.
        ELSEIF NOT vl_stcd2 IS INITIAL.
          MOVE vl_stcd2             TO st_zlest0019-cnpjferro.
        ELSE.
          MOVE wa_zlest0087_10-empresa   TO st_zlest0019-cnpjferro.
        ENDIF.
        MOVE vl_name1                    TO st_zlest0019-nomempferro.

        APPEND st_zlest0019 TO t_zlest0019.

        wa_zlest0087-status = 'P'.
        MODIFY ti_zlest0087 FROM wa_zlest0087 INDEX tabix TRANSPORTING status.
        flag = 'X'.

        "
        CLEAR: sl_zlest0035,vl_stcd1_2.

        SELECT SINGLE stcd1
             INTO vl_stcd1_2
             FROM lfa1
            WHERE lifnr = wa_zlest0087-lifnr.

        sl_zlest0035-nr_nf      = wa_zlest0041-nr_nf_propria.
        sl_zlest0035-serie_nf   = wa_zlest0041-serie_propria.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = sl_zlest0035-serie_nf
          IMPORTING
            output = sl_zlest0035-serie_nf.

        sl_zlest0035-docnum     = vl_docnum.

        "
        sl_zlest0035-cnpj       = vl_stcd1_2.
        sl_zlest0035-werks      = wa_zlest0087-werks.
        sl_zlest0035-qtd_nf     = wa_zlest0087-peso_nf.
        sl_zlest0035-qtd_cheg   = wa_zlest0087-descarga_rodo.
        sl_zlest0035-saldo      = wa_zlest0087-descarga_rodo.
        sl_zlest0035-dtachegada = wa_zlest0087-dt_descarga.

        APPEND sl_zlest0035 TO t_zlest0035.

      ENDIF.

    ENDLOOP.
    "REG10
    IF flag = 'X'.
      CLEAR: vl_name1, vl_stcd1, vl_stcd2.
      SELECT SINGLE name1 stcd1 stcd2
            INTO (vl_name1, vl_stcd1, vl_stcd2)
            FROM lfa1
           WHERE stcd1 = wa_zlest0087_10-cnpj_prest_serv.

      CLEAR st_zlest0019.

      vl_id_refkey = wa_zlest0087_10-id_refkey.

      MOVE:
      sy-mandt                    TO st_zlest0019-mandt,
      c_l1                        TO st_zlest0019-idinter,
      c_e                         TO st_zlest0019-tp_movi,
      wa_zlest0087_10-tp_reg      TO st_zlest0019-tp_reg,
      vl_name1                    TO st_zlest0019-nomempferro,
      wa_zlest0087_10-dtaenvio    TO st_zlest0019-dtaenvio,
      wa_zlest0087_10-horaenvio   TO st_zlest0019-horaenvio,
      "ST_REG10-OBSERV             TO ST_ZLEST0019-OBS,
      sy-datum                    TO st_zlest0019-erdat,
      sy-uzeit                    TO st_zlest0019-erzet,
      sy-uname                    TO st_zlest0019-uname,
      wa_zlest0087_10-id_refkey   TO st_zlest0019-id_zlest0019,
      wa_zlest0087_10-chave       TO st_zlest0019-chave.

      IF NOT vl_stcd1 IS INITIAL.
        MOVE vl_stcd1             TO st_zlest0019-cnpjferro.
      ELSEIF NOT vl_stcd2 IS INITIAL.
        MOVE vl_stcd2             TO st_zlest0019-cnpjferro.
      ELSE.
        MOVE wa_zlest0087_10-empresa   TO st_zlest0019-cnpjferro.
      ENDIF.

      vl_cnpjferro = st_zlest0019-cnpjferro.

*      SHIFT VL_ID_REFKEY LEFT DELETING LEADING '0'.
*      CONCATENATE SY-REPID WA_ZLEST0087_10-EMPRESA VL_ID_REFKEY INTO ST_ZLEST0019-CHAVE SEPARATED BY SPACE.
*      CONDENSE    ST_ZLEST0019-CHAVE NO-GAPS.

      vl_id_refkey = st_zlest0019-id_zlest0019.

      APPEND st_zlest0019 TO t_zlest0019.
    ENDIF.

  ENDLOOP.

  LOOP AT ti_zlest0087_10 INTO wa_zlest0087_10.
    tabix = sy-tabix.
    CLEAR flag.
    LOOP AT ti_zlest0087 INTO wa_zlest0087 WHERE id_refkey = wa_zlest0087_10-id_refkey.
      IF wa_zlest0087-status = ''.
        flag = 'X'.
      ELSE.
        UPDATE zlest0087 SET status = 'P'
          WHERE  idinter = wa_zlest0087-idinter
          AND    tp_movi = wa_zlest0087-tp_movi
          AND    tp_reg  = wa_zlest0087-tp_reg
          AND    chave   = wa_zlest0087-chave.
      ENDIF.
    ENDLOOP.

    IF flag IS INITIAL.
      wa_zlest0087_10-status = 'P'.
      MODIFY ti_zlest0087_10 FROM wa_zlest0087_10 INDEX tabix TRANSPORTING status.
      UPDATE zlest0087 SET status = 'P'
      WHERE  idinter = wa_zlest0087_10-idinter
      AND    tp_movi = wa_zlest0087_10-tp_movi
      AND    tp_reg  = wa_zlest0087_10-tp_reg
      AND    chave   = wa_zlest0087_10-chave.

    ENDIF.
  ENDLOOP.

  MODIFY zlest0019 FROM TABLE t_zlest0019.
  MODIFY zlest0035 FROM TABLE t_zlest0035.
  COMMIT WORK.

  "

*   V_MENSAGEM = 'Controle de balança importados com sucesso!'.
*   PERFORM ENVIA_MENSAGEM_PROCTO USING P1_FILE
*                                        C_S
*                                        '006'
*                                        V_MENSAGEM.
ENDFORM.                    " F_PROCESSA_TERCEIRO
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_TERCEIRO_L2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_terceiro_l2 .
  DATA: vl_bukrs    TYPE j_1bbranch-bukrs,
        vl_branch   TYPE j_1bbranch-branch,
        vl_nfnum    TYPE j_1bnfdoc-nfnum,
        vl_nfenum   TYPE j_1bnfdoc-nfenum,
        vl_nfnuma   TYPE j_1bnfdoc-nfnum,
        vl_nfenuma  TYPE j_1bnfdoc-nfenum,
        vl_cont_aux TYPE sy-index,
        flag(1).

  SELECT *
     FROM zlest0088
     INTO TABLE ti_zlest0088_10
     WHERE tp_reg = '10'
     AND   idinter = 'L2'
     AND   status = ''.

  CHECK ti_zlest0088_10[] IS NOT INITIAL.

  SELECT *
  FROM zlest0088
  INTO TABLE ti_zlest0088_20
  FOR ALL ENTRIES IN ti_zlest0088_10
  WHERE id_refkey = ti_zlest0088_10-id_refkey
  AND   tp_reg = '20'
  AND   idinter = 'L2'
  AND   status = ''.

  SELECT *
  FROM zlest0088
  INTO TABLE ti_zlest0088
  FOR ALL ENTRIES IN ti_zlest0088_10
  WHERE id_refkey = ti_zlest0088_10-id_refkey
  AND   tp_reg = '30'
  AND   idinter = 'L2'
  AND   status = ''.


  REFRESH t_zlest0019.

  SORT: ti_zlest0088_10 BY id_refkey,
        ti_zlest0088    BY id_refkey,
        ti_zlest0088_20 BY id_refkey dcl.

  LOOP AT ti_zlest0088_10 INTO wa_zlest0088_10.
    CLEAR flag.
    LOOP AT ti_zlest0088 INTO wa_zlest0088 WHERE id_refkey = wa_zlest0088_10-id_refkey.
      "REG30
      tabix = sy-tabix.

      CLEAR: vl_bukrs, vl_branch, vl_nfenum, vl_nfnum, vl_nfenuma, vl_nfnuma.

* Filial
      SELECT bukrs branch stcd1
      FROM j_1bbranch
      INTO TABLE t_branch_aux
      WHERE stcd1 = wa_zlest0088_10-cnpj_lifnr.


      IF sy-subrc IS INITIAL.

        DESCRIBE TABLE t_branch_aux LINES vl_cont_aux.

        " Conforme chamado quando encontrar 2 branch ignorar o 0001
        IF vl_cont_aux > 1.
          DELETE t_branch_aux WHERE branch EQ '0001'.
        ENDIF.

        READ TABLE t_branch_aux INTO wa_branch_aux WITH KEY stcd1 = wa_zlest0088-cnpj_lifnr.

        vl_bukrs =  wa_branch_aux-bukrs.
        vl_branch = wa_branch_aux-branch.
      ENDIF.

      IF vl_bukrs IS INITIAL.
        vl_bukrs  = wa_zlest0088-bukrs .
        vl_branch = wa_zlest0088-werks.
      ENDIF.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_ZLEST0088-NR_NF
*        IMPORTING
*          OUTPUT = VL_NFNUMA.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zlest0088-nr_nf
        IMPORTING
          output = vl_nfenuma.

** Documento de nota fiscal eletrônica
*      SELECT SINGLE NFENUM
*        INTO VL_NFENUM
*        FROM J_1BNFDOC
*       WHERE NFENUM = VL_NFENUMA
*         AND BUKRS  = VL_BUKRS
*         AND BRANCH = VL_BRANCH
*         AND CANCEL <> C_X.

** Documento de nota fiscal
*      SELECT SINGLE NFNUM
*        INTO VL_NFNUM
*        FROM J_1BNFDOC
*       WHERE NFNUM  = VL_NFNUMA
*         AND BUKRS  = VL_BUKRS
*         AND BRANCH = VL_BRANCH
*         AND CANCEL <> C_X.

      vl_nfenum = vl_nfenuma.
      SELECT SINGLE *
        FROM zlest0087
        INTO wa_zlest0087
        WHERE idinter   = 'L1'
        AND   nr_nf   = vl_nfenum
        AND   lifnr   = wa_zlest0088-lifnr
        AND   status  = 'P'. "Já gravou na 19 com L1.

      IF sy-subrc = 0.
        CLEAR: wa_zlest0041,st_zlest0019,vl_nfnum.
        SELECT SINGLE *
            INTO wa_zlest0041
            FROM zlest0041
           WHERE nr_nf       = wa_zlest0088-nr_nf
             AND cod_cliente = wa_zlest0088-lifnr.
        IF sy-subrc = 0.
          vl_nfenum = wa_zlest0041-nr_nf_propria.
        ENDIF.

        READ TABLE ti_zlest0088_20 INTO wa_zlest0088_20 WITH KEY id_refkey = wa_zlest0088_10-id_refkey dcl = wa_zlest0088-dcl BINARY SEARCH.
        IF sy-subrc = 0.
          IF  wa_zlest0088_20-status NE 'P'.
            wa_zlest0088_20-status = 'P'.
            MODIFY ti_zlest0088_20 FROM wa_zlest0088_20 INDEX sy-tabix TRANSPORTING status.
            "
            CLEAR: st_zlest0019.
            PERFORM zgeranumero(zlesi0006) CHANGING st_zlest0019-id_zlest0019.
            MOVE:
            sy-mandt                    TO st_zlest0019-mandt,
            'L2'                        TO st_zlest0019-idinter,
            'S'                         TO st_zlest0019-tp_movi,
            wa_zlest0088_20-tp_reg      TO st_zlest0019-tp_reg,
            wa_zlest0088_20-dcl         TO st_zlest0019-dcl,
            wa_zlest0088_20-seriedcl    TO st_zlest0019-seriedcl,
            " Exclusivo REG20
            wa_zlest0088_20-idvagao     TO st_zlest0019-idvagao,
            wa_zlest0088_20-peso_vagao  TO st_zlest0019-pesovagao,
            wa_zlest0088_20-dt_ferro    TO st_zlest0019-dtadecarga,
            wa_zlest0088_20-hora_ferro  TO st_zlest0019-horadescarga,

            sy-datum                    TO st_zlest0019-erdat,
            sy-uzeit                    TO st_zlest0019-erzet,
            sy-uname                    TO st_zlest0019-uname,
            wa_zlest0088_20-id_refkey   TO st_zlest0019-id_refkey,
            wa_zlest0088_10-cnpj_prest_serv    TO st_zlest0019-cnpjferro,
            wa_zlest0088_20-chave       TO st_zlest0019-chave.

*            CONCATENATE  WA_ZLEST0088_20-DCL WA_ZLEST0088_20-IDVAGAO INTO ST_ZLEST0019-CHAVE SEPARATED BY SPACE.
*            CONDENSE ST_ZLEST0019-CHAVE NO-GAPS.
            IF st_zlest0019-dcl IS NOT INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = st_zlest0019-dcl
                IMPORTING
                  output = st_zlest0019-dcl.
            ENDIF.

            APPEND st_zlest0019 TO t_zlest0019.
          ENDIF.
        ENDIF.

        CLEAR wa_zlest0089.
        SELECT SINGLE *
         FROM zlest0089
         INTO wa_zlest0089
         WHERE matnr  = wa_zlest0087-produto
         AND   bukrs  = wa_zlest0087-bukrs.

        CLEAR: st_zlest0019.
        PERFORM zgeranumero(zlesi0006) CHANGING st_zlest0019-id_zlest0019.
        MOVE:
        sy-mandt                    TO st_zlest0019-mandt,
        'L2'                        TO st_zlest0019-idinter,
        'S'                         TO st_zlest0019-tp_movi,
        wa_zlest0088-cnpj_lifnr     TO st_zlest0019-cnpjcliente,
        wa_zlest0088-tp_reg         TO st_zlest0019-tp_reg,
        wa_zlest0088-dcl            TO st_zlest0019-dcl,
        wa_zlest0088-seriedcl       TO st_zlest0019-seriedcl,
        " Exclusivo REG20
        wa_zlest0088-idvagao        TO st_zlest0019-idvagao,
        wa_zlest0088-peso_vagao     TO st_zlest0019-pesovagao,
        wa_zlest0088-dt_ferro       TO st_zlest0019-dtadecarga,
        wa_zlest0088-hora_ferro     TO st_zlest0019-horadescarga,
        " Exclusivo REG30
        wa_zlest0088-bukrs          TO st_zlest0019-bukrs,
        wa_zlest0088-werks          TO st_zlest0019-branch,
        vl_nfenum                   TO st_zlest0019-nfenum,
        "WA_ZLEST0088-SERIE          TO ST_ZLEST0019-
        wa_zlest0088-peso_nf        TO st_zlest0019-pesonf,
        wa_zlest0088-peso_nf_vagao  TO st_zlest0019-pesodvagao,
        wa_zlest0088-dtachegada     TO st_zlest0019-dtachegada,
        wa_zlest0088-id_refkey      TO st_zlest0019-id_refkey,

        wa_zlest0041-cod_cliente    TO st_zlest0019-cod_fornecedor,
        wa_zlest0041-nr_nf          TO st_zlest0019-nr_nf_terceiro,
        wa_zlest0089-produto        TO st_zlest0019-produto,

        sy-datum                    TO st_zlest0019-erdat,
        sy-uzeit                    TO st_zlest0019-erzet,
        sy-uname                    TO st_zlest0019-uname,
        wa_zlest0088-id_refkey      TO st_zlest0019-id_refkey,
        wa_zlest0088_10-cnpj_prest_serv    TO st_zlest0019-cnpjferro.
        "WA_ZLEST0088-CHAVE          TO ST_ZLEST0019-CHAVE.

        CONCATENATE wa_zlest0088-bukrs '-' wa_zlest0088-werks '-' vl_nfenum vl_nfnum INTO st_zlest0019-chave SEPARATED BY space.
        CONDENSE st_zlest0019-chave NO-GAPS.

        IF st_zlest0019-dcl IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = st_zlest0019-dcl
            IMPORTING
              output = st_zlest0019-dcl.
        ENDIF.

        APPEND st_zlest0019 TO t_zlest0019.
        "
        wa_zlest0088-status = 'P'.
        MODIFY ti_zlest0088 FROM wa_zlest0088 INDEX tabix TRANSPORTING status.
        flag = 'X'.
      ENDIF.
    ENDLOOP.
    "REG10
    IF flag = 'X'.
      CLEAR: vl_name1, vl_stcd1, vl_stcd2.
      SELECT SINGLE name1 stcd1 stcd2
            INTO (vl_name1, vl_stcd1, vl_stcd2)
            FROM lfa1
           WHERE stcd1 = wa_zlest0088_10-cnpj_prest_serv.


      CLEAR st_zlest0019.

      vl_id_refkey = wa_zlest0088_10-id_refkey.

      MOVE:
      sy-mandt                    TO st_zlest0019-mandt,
      'L2'                        TO st_zlest0019-idinter,
      'S'                         TO st_zlest0019-tp_movi,
      wa_zlest0088_10-tp_reg      TO st_zlest0019-tp_reg,
      vl_name1                    TO st_zlest0019-nomempferro,
      wa_zlest0088_10-dtaenvio    TO st_zlest0019-dtaenvio,
      wa_zlest0088_10-horaenvio   TO st_zlest0019-horaenvio,
      "ST_REG10-OBSERV             TO ST_ZLEST0019-OBS,
      sy-datum                    TO st_zlest0019-erdat,
      sy-uzeit                    TO st_zlest0019-erzet,
      sy-uname                    TO st_zlest0019-uname,
      vl_id_refkey                TO st_zlest0019-id_zlest0019,
      wa_zlest0088_10-chave       TO st_zlest0019-chave.

      "Se não tiver a etapa 1 não deixar importar a estapa 2

      IF NOT vl_stcd1 IS INITIAL.
        MOVE vl_stcd1             TO st_zlest0019-cnpjferro.
      ELSEIF NOT vl_stcd2 IS INITIAL.
        MOVE vl_stcd2             TO st_zlest0019-cnpjferro.
      ENDIF.

      vl_cnpjferro = st_zlest0019-cnpjferro.

*      SHIFT VL_ID_REFKEY LEFT DELETING LEADING '0'.
*      CONCATENATE SY-REPID WA_ZLEST0088_10-EMPRESA VL_ID_REFKEY INTO ST_ZLEST0019-CHAVE SEPARATED BY SPACE.
*      CONDENSE    ST_ZLEST0019-CHAVE NO-GAPS.
*      VL_ID_REFKEY = ST_ZLEST0019-ID_ZLEST0019.

      APPEND st_zlest0019 TO t_zlest0019.
    ENDIF.

  ENDLOOP.

  LOOP AT ti_zlest0088_10 INTO wa_zlest0088_10.
    tabix = sy-tabix.
    CLEAR flag.
    LOOP AT ti_zlest0088 INTO wa_zlest0088 WHERE id_refkey = wa_zlest0088_10-id_refkey.
      IF wa_zlest0088-status = ''.
        flag = 'X'.
      ELSE.
        UPDATE zlest0088 SET status = 'P'
        WHERE  idinter   = wa_zlest0088-idinter
        AND    tp_movi   = wa_zlest0088-tp_movi
        AND    tp_reg    = wa_zlest0088-tp_reg
        AND    chave     = wa_zlest0088-chave
        AND    dcl       = wa_zlest0088-dcl
        AND    seriedcl  = wa_zlest0088-seriedcl.

      ENDIF.
    ENDLOOP.

    LOOP AT ti_zlest0088_20 INTO wa_zlest0088 WHERE id_refkey = wa_zlest0088_10-id_refkey.
      IF wa_zlest0088-status = ''.
        flag = 'X'.
      ELSE.
        UPDATE zlest0088 SET status = 'P'
        WHERE  idinter   = wa_zlest0088-idinter
        AND    tp_movi   = wa_zlest0088-tp_movi
        AND    tp_reg    = wa_zlest0088-tp_reg
        AND    chave     = wa_zlest0088-chave
        AND    dcl       = wa_zlest0088-dcl
        AND    seriedcl  = wa_zlest0088-seriedcl.
      ENDIF.
    ENDLOOP.

    IF flag IS INITIAL.
      wa_zlest0088_10-status = 'P'.
      MODIFY ti_zlest0088_10 FROM wa_zlest0088_10 INDEX tabix TRANSPORTING status.
      UPDATE zlest0088 SET status = 'P'
      WHERE  idinter  = wa_zlest0088_10-idinter
      AND    tp_movi  = wa_zlest0088_10-tp_movi
      AND    tp_reg   = wa_zlest0088_10-tp_reg
      AND    chave    = wa_zlest0088_10-chave
      AND    dcl      = wa_zlest0088_10-dcl
      AND    seriedcl = wa_zlest0088_10-seriedcl.
    ENDIF.
  ENDLOOP.

  MODIFY zlest0019 FROM TABLE t_zlest0019.
  COMMIT WORK.
ENDFORM.                    " F_PROCESSA_TERCEIRO_L2
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_TERCEIRO_L3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_terceiro_l3 .
  DATA: vl_bukrs    TYPE j_1bbranch-bukrs,
        vl_branch   TYPE j_1bbranch-branch,
        vl_nfnum    TYPE j_1bnfdoc-nfnum,
        vl_nfenum   TYPE j_1bnfdoc-nfenum,
        vl_nfnuma   TYPE j_1bnfdoc-nfnum,
        vl_nfenuma  TYPE j_1bnfdoc-nfenum,
        vl_cont_aux TYPE sy-index,
        flag(1).

  REFRESH: ti_zlest0088_10,ti_zlest0088_20,ti_zlest0088.

  SELECT *
     FROM zlest0088
     INTO TABLE ti_zlest0088_10
     WHERE tp_reg = '10'
     AND   idinter = 'L3'
     AND   status = ''.

  CHECK ti_zlest0088_10[] IS NOT INITIAL.

  SELECT *
  FROM zlest0088
  INTO TABLE ti_zlest0088_20
  FOR ALL ENTRIES IN ti_zlest0088_10
  WHERE id_refkey = ti_zlest0088_10-id_refkey
  AND   tp_reg = '20'
  AND   idinter = 'L3'
  AND   status = ''.

  SELECT *
  FROM zlest0088
  INTO TABLE ti_zlest0088
  FOR ALL ENTRIES IN ti_zlest0088_10
  WHERE id_refkey = ti_zlest0088_10-id_refkey
  AND   tp_reg = '30'
  AND   idinter = 'L3'
  AND   status = ''.


  REFRESH t_zlest0019.

  SORT: ti_zlest0088_10 BY id_refkey,
        ti_zlest0088    BY id_refkey,
        ti_zlest0088_20 BY id_refkey dcl.

  LOOP AT ti_zlest0088_10 INTO wa_zlest0088_10.
    CLEAR flag.
    LOOP AT ti_zlest0088 INTO wa_zlest0088 WHERE id_refkey = wa_zlest0088_10-id_refkey.
      "REG30
      tabix = sy-tabix.

      CLEAR: vl_bukrs, vl_branch, vl_nfenum, vl_nfnum, vl_nfenuma, vl_nfnuma.


      SELECT SINGLE bukrs branch
      INTO (vl_bukrs, vl_branch)
      FROM j_1bbranch
      WHERE stcd1 = wa_zlest0088_10-cnpj_lifnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zlest0088-nr_nf
        IMPORTING
          output = vl_nfnuma.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zlest0088-nr_nf
        IMPORTING
          output = vl_nfenuma.

** Documento de nota fiscal eletrônica
*      SELECT SINGLE NFENUM
*      INTO  VL_NFENUM
*      FROM  J_1BNFDOC
*      WHERE NFENUM = VL_NFENUMA
*      AND   BUKRS  = VL_BUKRS
*      AND   BRANCH = VL_BRANCH
*      AND   CANCEL <> C_X.

      "----------------------------------------
      CLEAR st_zlest0019.

      SELECT SINGLE *
        FROM zlest0088
        INTO wa_zlest0088_2
        WHERE idinter   = 'L2'
        AND   nr_nf   = vl_nfenuma "VL_NFENUM
        AND   lifnr   = wa_zlest0088-lifnr
        AND   status  = 'P'. "Já gravou na 19 com L2.

      IF sy-subrc = 0.
        CLEAR: wa_zlest0041,st_zlest0019,vl_nfnum.
        SELECT SINGLE *
            INTO wa_zlest0041
            FROM zlest0041
           WHERE nr_nf       = wa_zlest0088-nr_nf
             AND cod_cliente = wa_zlest0088-lifnr.
        IF sy-subrc = 0.
          vl_nfenum = wa_zlest0041-nr_nf_propria.
        ENDIF.

        READ TABLE ti_zlest0088_20 INTO wa_zlest0088_20 WITH KEY id_refkey = wa_zlest0088_10-id_refkey dcl = wa_zlest0088-dcl BINARY SEARCH.
        IF sy-subrc = 0.
          IF  wa_zlest0088_20-status NE 'P'.
            wa_zlest0088_20-status = 'P'.
            MODIFY ti_zlest0088_20 FROM wa_zlest0088_20 INDEX sy-tabix TRANSPORTING status.
            "
            CLEAR: st_zlest0019.
            PERFORM zgeranumero(zlesi0006) CHANGING st_zlest0019-id_zlest0019.
            MOVE:
            sy-mandt                    TO st_zlest0019-mandt,
            'L3'                        TO st_zlest0019-idinter,
            'E'                         TO st_zlest0019-tp_movi,
            wa_zlest0088_20-tp_reg      TO st_zlest0019-tp_reg,
            wa_zlest0088_20-dcl         TO st_zlest0019-dcl,
            wa_zlest0088_20-seriedcl    TO st_zlest0019-seriedcl,
            " Exclusivo REG20
            wa_zlest0088_20-idvagao     TO st_zlest0019-idvagao,
            wa_zlest0088_20-peso_vagao  TO st_zlest0019-pesovagao,
            wa_zlest0088_20-dt_ferro    TO st_zlest0019-dtadecarga,
            wa_zlest0088_20-hora_ferro  TO st_zlest0019-horadescarga,

            sy-datum                    TO st_zlest0019-erdat,
            sy-uzeit                    TO st_zlest0019-erzet,
            sy-uname                    TO st_zlest0019-uname,
            wa_zlest0088_20-id_refkey   TO st_zlest0019-id_refkey,
            wa_zlest0088_10-cnpj_prest_serv    TO st_zlest0019-cnpjferro,
            wa_zlest0088_20-chave       TO st_zlest0019-chave.

*            CONCATENATE  WA_ZLEST0088_20-DCL WA_ZLEST0088_20-IDVAGAO INTO ST_ZLEST0019-CHAVE SEPARATED BY SPACE.
*            CONDENSE ST_ZLEST0019-CHAVE NO-GAPS.

            IF st_zlest0019-dcl IS NOT INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = st_zlest0019-dcl
                IMPORTING
                  output = st_zlest0019-dcl.
            ENDIF.

            APPEND st_zlest0019 TO t_zlest0019.
          ENDIF.
        ENDIF.

        CLEAR wa_zlest0089.
        SELECT SINGLE *
         FROM zlest0089
         INTO wa_zlest0089
         WHERE matnr  = wa_zlest0087-produto
         AND   bukrs  = wa_zlest0087-bukrs.

        CLEAR: st_zlest0019.
        PERFORM zgeranumero(zlesi0006) CHANGING st_zlest0019-id_zlest0019.
        MOVE:
        sy-mandt                    TO st_zlest0019-mandt,
        'L3'                        TO st_zlest0019-idinter,
        'E'                         TO st_zlest0019-tp_movi,
        wa_zlest0088-tp_reg         TO st_zlest0019-tp_reg,
        wa_zlest0088-cnpj_lifnr     TO st_zlest0019-cnpjcliente,
        wa_zlest0088-dcl            TO st_zlest0019-dcl,
        wa_zlest0088-seriedcl       TO st_zlest0019-seriedcl,
        " Exclusivo REG20
        wa_zlest0088-idvagao        TO st_zlest0019-idvagao,
        wa_zlest0088-peso_vagao     TO st_zlest0019-pesovagao,
        wa_zlest0088-dt_ferro       TO st_zlest0019-dtadecarga,
        wa_zlest0088-hora_ferro     TO st_zlest0019-horadescarga,
        " Exclusivo REG30
        wa_zlest0088-bukrs          TO st_zlest0019-bukrs,
        wa_zlest0088-werks          TO st_zlest0019-branch,
        vl_nfenum                   TO st_zlest0019-nfenum,
        "WA_ZLEST0088-SERIE          TO ST_ZLEST0019-
        wa_zlest0088-peso_nf        TO st_zlest0019-pesonf,
        wa_zlest0088-peso_nf_vagao  TO st_zlest0019-pesodvagao,
        wa_zlest0088-dtachegada     TO st_zlest0019-dtachegada,
        wa_zlest0088-id_refkey      TO st_zlest0019-id_refkey,

        wa_zlest0041-cod_cliente    TO st_zlest0019-cod_fornecedor,
        wa_zlest0041-nr_nf          TO st_zlest0019-nr_nf_terceiro,

        wa_zlest0089-produto        TO st_zlest0019-produto,
        sy-datum                    TO st_zlest0019-erdat,
        sy-uzeit                    TO st_zlest0019-erzet,
        sy-uname                    TO st_zlest0019-uname,
        wa_zlest0088-id_refkey      TO st_zlest0019-id_refkey,
        wa_zlest0088_10-cnpj_prest_serv    TO st_zlest0019-cnpjferro.
*        WA_ZLEST0088-CHAVE          TO ST_ZLEST0019-CHAVE.

        CONCATENATE wa_zlest0088-bukrs '-' wa_zlest0088-werks '-' vl_nfenum vl_nfnum INTO st_zlest0019-chave SEPARATED BY space.
        CONDENSE st_zlest0019-chave NO-GAPS.

        IF st_zlest0019-dcl IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = st_zlest0019-dcl
            IMPORTING
              output = st_zlest0019-dcl.
        ENDIF.

        APPEND st_zlest0019 TO t_zlest0019.
        "
        wa_zlest0088-status = 'P'.
        MODIFY ti_zlest0088 FROM wa_zlest0088 INDEX tabix TRANSPORTING status.
        flag = 'X'.
      ENDIF.
    ENDLOOP.
    "REG10
    IF flag = 'X'.
      CLEAR: vl_name1, vl_stcd1, vl_stcd2.
      SELECT SINGLE name1 stcd1 stcd2
            INTO (vl_name1, vl_stcd1, vl_stcd2)
            FROM lfa1
           WHERE stcd1 = wa_zlest0088_10-cnpj_prest_serv.


      CLEAR st_zlest0019.

      vl_id_refkey = wa_zlest0088_10-id_refkey.

      MOVE:
      sy-mandt                    TO st_zlest0019-mandt,
      'L3'                        TO st_zlest0019-idinter,
      'E'                         TO st_zlest0019-tp_movi,
      wa_zlest0088_10-tp_reg      TO st_zlest0019-tp_reg,
      vl_name1                    TO st_zlest0019-nomempferro,
      wa_zlest0088_10-dtaenvio    TO st_zlest0019-dtaenvio,
      wa_zlest0088_10-horaenvio   TO st_zlest0019-horaenvio,
      "ST_REG10-OBSERV             TO ST_ZLEST0019-OBS,
      sy-datum                    TO st_zlest0019-erdat,
      sy-uzeit                    TO st_zlest0019-erzet,
      sy-uname                    TO st_zlest0019-uname,
      vl_id_refkey                TO st_zlest0019-id_zlest0019,
      wa_zlest0088_10-chave       TO st_zlest0019-chave.

      "Se não tiver a etapa 1 não deixar importar a estapa 2

      IF NOT vl_stcd1 IS INITIAL.
        MOVE vl_stcd1             TO st_zlest0019-cnpjferro.
      ELSEIF NOT vl_stcd2 IS INITIAL.
        MOVE vl_stcd2             TO st_zlest0019-cnpjferro.
      ENDIF.

      vl_cnpjferro = st_zlest0019-cnpjferro.

*      SHIFT VL_ID_REFKEY LEFT DELETING LEADING '0'.
*      CONCATENATE SY-REPID WA_ZLEST0088_10-EMPRESA VL_ID_REFKEY INTO ST_ZLEST0019-CHAVE SEPARATED BY SPACE.
*      CONDENSE    ST_ZLEST0019-CHAVE NO-GAPS.

      vl_id_refkey = st_zlest0019-id_zlest0019.

      APPEND st_zlest0019 TO t_zlest0019.
    ENDIF.

  ENDLOOP.

  LOOP AT ti_zlest0088_10 INTO wa_zlest0088_10.
    tabix = sy-tabix.
    CLEAR flag.
    LOOP AT ti_zlest0088 INTO wa_zlest0088 WHERE id_refkey = wa_zlest0088_10-id_refkey.
      IF wa_zlest0088-status = ''.
        flag = 'X'.
      ELSE.
        UPDATE zlest0088 SET status = 'P'
        WHERE  idinter   = wa_zlest0088-idinter
        AND    tp_movi   = wa_zlest0088-tp_movi
        AND    tp_reg    = wa_zlest0088-tp_reg
        AND    chave     = wa_zlest0088-chave
        AND    dcl       = wa_zlest0088-dcl
        AND    seriedcl  = wa_zlest0088-seriedcl.
      ENDIF.
    ENDLOOP.

    LOOP AT ti_zlest0088_20 INTO wa_zlest0088 WHERE id_refkey = wa_zlest0088_10-id_refkey.
      IF wa_zlest0088-status = ''.
        flag = 'X'.
      ELSE.
        UPDATE zlest0088 SET status = 'P'
        WHERE  idinter   = wa_zlest0088-idinter
        AND    tp_movi   = wa_zlest0088-tp_movi
        AND    tp_reg    = wa_zlest0088-tp_reg
        AND    chave     = wa_zlest0088-chave
        AND    dcl       = wa_zlest0088-dcl
        AND    seriedcl  = wa_zlest0088-seriedcl.
      ENDIF.
    ENDLOOP.

    IF flag IS INITIAL.
      wa_zlest0088_10-status = 'P'.
      MODIFY ti_zlest0088_10 FROM wa_zlest0088_10 INDEX tabix TRANSPORTING status.
      UPDATE zlest0088 SET status = 'P'
      WHERE  idinter  = wa_zlest0088_10-idinter
      AND    tp_movi  = wa_zlest0088_10-tp_movi
      AND    tp_reg   = wa_zlest0088_10-tp_reg
      AND    chave    = wa_zlest0088_10-chave
      AND    dcl      = wa_zlest0088_10-dcl
      AND    seriedcl = wa_zlest0088_10-seriedcl.
    ENDIF.
  ENDLOOP.

  MODIFY zlest0019 FROM TABLE t_zlest0019.
  COMMIT WORK.
ENDFORM.                    " F_PROCESSA_TERCEIRO_L3

FORM f_check_depara_nf_cancel.

  DATA: lit_zlest0041_del TYPE TABLE OF zlest0041.

  DATA: lva_data_limite TYPE sy-datum.

  lva_data_limite = sy-datum - 90.

  CLEAR: lit_zlest0041_del[].

  SELECT *
    FROM zlest0041 AS a INTO TABLE lit_zlest0041_del
   WHERE data GE lva_data_limite
     AND NOT EXISTS ( SELECT docnum
                       FROM j_1bnfe_active AS b
                      WHERE b~docnum = a~docnum
                        AND docsta EQ '1'
                        AND scssta NE '2' ).


  LOOP AT lit_zlest0041_del INTO DATA(lwa_zlest0041_del).
    DELETE FROM zlest0041 WHERE centro_comprador = lwa_zlest0041_del-centro_comprador
                            AND nr_nf            = lwa_zlest0041_del-nr_nf
                            AND cod_cliente      = lwa_zlest0041_del-cod_cliente
                            AND serie            = lwa_zlest0041_del-serie.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.
