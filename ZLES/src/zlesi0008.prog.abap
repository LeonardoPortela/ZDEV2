*----------------------------------------------------------------------*
*                             AMAGGI                                   *
*----------------------------------------------------------------------*
* Cliente    : Grupo Andre Maggi                                       *
* Autor      : BBKO Consulting S.A.                                    *
* Data       : 08/09/2010                                              *
* Descrição  :                                                         *
* Transação  :                                                         *
* Projeto    : Projeto Evoluir                                         *
* Cód Espec. :                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Em:        | Por:         | Alteração:                               *
*------------+--------------+------------------------------------------*
* 08/09/2010 | Mateus       | Desenvolvimento inicial                  *
*----------------------------------------------------------------------*

REPORT  zlesi0008 MESSAGE-ID lzes.

TYPES: BEGIN OF ty_zlest0004,
         serie_despacho TYPE zlest0004-serie_despacho,
         nr_despacho    TYPE zlest0004-nr_despacho,
         nr_fatura      TYPE zlest0004-nr_fatura,
         cgc_remetente  TYPE zlest0004-cgc_remetente,
         cgc_dest       TYPE zlest0004-cgc_dest,
         emissao        TYPE zlest0004-emissao,
         x(2)           TYPE c,
       END OF ty_zlest0004,

       BEGIN OF ty_zlesbranch,
         serie_despacho TYPE zlest0004-serie_despacho,
         nr_despacho    TYPE zlest0004-nr_despacho,
         nr_fatura      TYPE zlest0004-nr_fatura,
         stcd1          TYPE j_1bbranch-stcd1,
       END OF ty_zlesbranch,

       BEGIN OF ty_zlest0003,
         serie_despacho TYPE zlest0003-serie_despacho,
         nr_despacho    TYPE zlest0003-nr_despacho,
         nr_nf          TYPE zlest0003-nr_nf,
         cgc_remetente  TYPE zlest0035-cnpj,
         branch         TYPE j_1bbranch-branch,
       END OF ty_zlest0003,

       BEGIN OF ty_zlest0003_aux,
         serie_despacho TYPE zlest0003-serie_despacho,
         nr_despacho    TYPE zlest0003-nr_despacho,
         nr_nf          TYPE zlest0003-nr_nf,
         cgc_remetente  TYPE zlest0035-cnpj,
         cgc_cliente    TYPE zlest0006-cgc_cliente,
       END OF ty_zlest0003_aux,

       BEGIN OF ty_j_1bbranch,
         bukrs  TYPE j_1bbranch-bukrs,
         branch TYPE j_1bbranch-branch,
         stcd1  TYPE j_1bbranch-stcd1,
       END OF ty_j_1bbranch,

       BEGIN OF ty_j_1bbranch_aux,
         x(18) TYPE c,
         stcd1 TYPE lfa1-stcd1,
       END OF ty_j_1bbranch_aux,

       BEGIN OF ty_j_1bnfdoc,
         docnum TYPE j_1bnfdoc-docnum,
         nfnum  TYPE j_1bnfdoc-nfnum,
         bukrs  TYPE j_1bnfdoc-bukrs,
         branch TYPE j_1bnfdoc-branch,
         nfenum TYPE j_1bnfdoc-nfenum,
         nfe    TYPE j_1bnfdoc-nfe,
         parvw  TYPE j_1bnfdoc-parvw,
         parid  TYPE j_1bnfdoc-parid,
       END OF ty_j_1bnfdoc,

       BEGIN OF ty_j_1bnfdoc_aux,
         nfnum  TYPE j_1bnfdoc-nfnum,
         nfenum TYPE j_1bnfdoc-nfenum,
         bukrs  TYPE j_1bnfdoc-bukrs,
         branch TYPE j_1bnfdoc-branch,
       END OF ty_j_1bnfdoc_aux,

       BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
         refitm TYPE j_1bnflin-refitm,
       END OF ty_j_1bnflin,

       BEGIN OF ty_j_1bnflin_aux,
         docnum TYPE j_1bnflin-docnum,
         vbeln  TYPE vbfa-vbeln,
         x(25)  TYPE c,
         posnn  TYPE vbfa-posnn,
       END OF ty_j_1bnflin_aux,

       BEGIN OF ty_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         vbtyp_v TYPE vbfa-vbtyp_v,
         vbtyp_n TYPE vbfa-vbtyp_n,
       END OF ty_vbfa,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         stcd1 TYPE lfa1-stcd1,
       END OF ty_lfa1,

       BEGIN OF type_msn,
         tp_msn         TYPE bapi_mtype,
         dacte          TYPE zlest0006-nr_nf_all,
         serie_despacho TYPE zsrdep,
         nr_despacho    TYPE znrdesp,
         cgc_remetente  TYPE zcgc_rem,
         ov             TYPE vbeln_va,
         nf             TYPE j_1bnfnum9,
         messagem       TYPE bapi_msg,
       END   OF type_msn.

DATA: ti_zlest0006      TYPE TABLE OF zlest0006,
      ti_zlest0005      TYPE TABLE OF zlest0005,
      ti_zlest0004      TYPE TABLE OF ty_zlest0004,
      ti_zlest0003      TYPE TABLE OF ty_zlest0003,
      ti_zlest0003_aux  TYPE TABLE OF ty_zlest0003_aux,
      ti_zlest0019      TYPE TABLE OF zlest0019,
      ti_zlest0019_nfe  TYPE TABLE OF zlest0019,
      ti_zlest0019_nf   TYPE TABLE OF zlest0019,
      ti_j_1bbranch     TYPE TABLE OF ty_j_1bbranch,
      ti_j_1bbranch_aux TYPE TABLE OF ty_zlesbranch,
      ti_j_1bnfdoc      TYPE TABLE OF ty_j_1bnfdoc,
      ti_j_1bnfdoc_aux  TYPE TABLE OF ty_j_1bnfdoc_aux,
      ti_j_1bnflin      TYPE TABLE OF ty_j_1bnflin,
      ti_j_1bnflin_aux  TYPE TABLE OF ty_j_1bnflin_aux,
      ti_vbfa           TYPE TABLE OF ty_vbfa,
      ti_vbfa_aux       TYPE TABLE OF ty_vbfa,
      ti_itemdata       TYPE TABLE OF bapishipmentitem,
      ti_stagedata      TYPE TABLE OF bapishipmentstage,
      ti_return         TYPE TABLE OF bapiret2,
      ti_zlest0008      TYPE TABLE OF zlest0008,
      ti_bdc            TYPE TABLE OF bdcdata,
      ti_msg            TYPE TABLE OF bdcmsgcoll,
      ti_lfa1           TYPE TABLE OF ty_lfa1,
      ti_zlest0041      TYPE TABLE OF zlest0041,
      ti_lfa1_aux       TYPE TABLE OF ty_lfa1,
      ti_erros          TYPE TABLE OF type_msn,
      ti_zlest0035      TYPE TABLE OF zlest0035,
      t_save            TYPE TABLE OF zlest0035.

DATA: wa_zlest0006      TYPE zlest0006,
      wa_zlest0004      TYPE ty_zlest0004,
      wa_zlest0003      TYPE ty_zlest0003,
      wa_zlest0003_aux  TYPE ty_zlest0003_aux,
      wa_j_1bbranch     TYPE ty_j_1bbranch,
      wa_j_1bnfdoc      TYPE ty_j_1bnfdoc,
      wa_j_1bnfdoc_aux  TYPE ty_j_1bnfdoc_aux,
      wa_j_1bnflin      TYPE ty_j_1bnflin,
      wa_vbfa           TYPE ty_vbfa,
      wa_headerdata     TYPE bapishipmentheader,
      wa_itemdata       TYPE bapishipmentitem,
      wa_stagedata      TYPE bapishipmentstage,
      wa_j_1bbranch_aux TYPE ty_zlesbranch,
      wa_return         TYPE bapiret2,
      wa_zlest0008      TYPE zlest0008,
      wa_bdc            TYPE bdcdata,
      wa_msg            TYPE bdcmsgcoll,
      wa_lfa1           TYPE ty_lfa1,
      wa_lfa1_2         TYPE ty_lfa1,
      wa_zlest0041      TYPE zlest0041,
      wa_lfa1_aux       TYPE ty_lfa1.

DATA: v_tknum   TYPE vttk-tknum,
      v_fknum   TYPE fknum,
      v_tabix   TYPE sy-tabix,
      v_erro    TYPE c,
      v_mode(1) TYPE c,
      v_message TYPE string.


CONSTANTS: c_l(1)      TYPE c              VALUE 'L',
           c_b(1)      TYPE c              VALUE 'B',
           c_e(1)      TYPE c              VALUE 'E',
           c_n(1)      TYPE c              VALUE 'N',
           c_m(1)      TYPE c              VALUE 'M',
           c_s(1)      TYPE c              VALUE 'S',
           c_j(1)      TYPE c              VALUE 'J',
           c_r(1)      TYPE c              VALUE 'R',
           c_w(1)      TYPE c              VALUE 'W',
           c_x(1)      TYPE c              VALUE 'X',
           c_bi(2)     TYPE c              VALUE 'BI',
           c_vy(2)     TYPE c              VALUE 'VY',
           c_z003(4)   TYPE c              VALUE 'Z003',
           c_1(1)      TYPE c              VALUE '1',
           c_02(2)     TYPE c              VALUE '02',
           c_30(2)     TYPE c              VALUE '30',
           c_007(3)    TYPE c              VALUE '007',
           c_h(1)      TYPE c              VALUE 'H',
           c_0001(4)   TYPE c              VALUE '0001',
           c_tgg(3)    TYPE c              VALUE 'TGG',
           c_mga(3)    TYPE c              VALUE 'MGA',
           c_vi01      TYPE sy-tcode       VALUE 'VI01',
           c_01        TYPE inri-nrrangenr VALUE '01',
           cn_zeros(9) TYPE n              VALUE '000000000',
           c_zles0008  TYPE inri-object    VALUE 'ZLES0008'.

TABLES zlest0006.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_all FOR zlest0006-nr_nf_all ."OBLIGATORY.

  PARAMETERS : p_cnpj TYPE zlest0006-cgc_all,
               trp    TYPE c AS CHECKBOX USER-COMMAND sel  DEFAULT 'X'.

SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME .
  PARAMETERS : p_part   TYPE vtts-knotz MODIF ID trp.
SELECTION-SCREEN: END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM hide_options.

START-OF-SELECTION.

  IF so_all IS INITIAL .
    MESSAGE i000(z04) WITH 'Informe a DACTE !'.
    EXIT.
  ENDIF.

  IF p_cnpj IS INITIAL .
    MESSAGE i000(z04) WITH 'Informe o CNPJ !'.
    EXIT.
  ENDIF.

  IF trp IS INITIAL AND p_part IS INITIAL .
    MESSAGE i000(z04) WITH 'Informe o Local de partida !'.
    EXIT.
  ENDIF.

* Seleciona Dados
  PERFORM: zf_seleciona_dados,
* Processa Dados
           zf_processa_dados,
* Atualiza Dados
           zf_atualiza_dados.

END-OF-SELECTION.

  IF NOT ti_erros[] IS INITIAL.
*   Exibe Erros
    PERFORM z_exibe_erros.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleção de Dados
*----------------------------------------------------------------------*
FORM zf_seleciona_dados .

  DATA:
    tl_zlest0003 TYPE TABLE OF ty_zlest0003,
    tl_stcd1     TYPE TABLE OF ty_zlesbranch.

  CLEAR v_erro.

* Faturas - Ferroviario
  PERFORM: z_seleciona_zlest0006 ,
* Despachos - Ferroviario
           z_seleciona_zlest0004 ,
* Vagões - Ferroviarios
           z_seleciona_zslest005 ,
* Peso Confirmado
           z_seleciona_zlest0019 ,
* NF Clientes
           z_seleciona_zlest0003 ,
* Controle Saldo Ferroviário
           z_seleciona_zlest0035 ,
* Local de Negocios
           z_seleciona_j_1bbranch,
* Cabeçalho NF's
           z_seleciona_j_1bnfdoc ,
* Seleciona Itens NF's
           z_seleciona_j_1bnflin ,
* Seleciona Remessas
           z_seleciona_vbfa      .

ENDFORM.                    " ZF_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       Processa registros
*----------------------------------------------------------------------*
FORM zf_processa_dados .

  DATA: tl_zlest0019 TYPE TABLE OF zlest0019,
        tl_aux       TYPE TABLE OF zlest0019,
        sl_aux       TYPE zlest0019,
        sl_zlest0019 TYPE zlest0019,
        sl_zlest0035 TYPE zlest0035,
        sl_zlest0005 TYPE zlest0005,
        vl_subrc     TYPE sy-subrc,
        vl_index     TYPE sytabix,
        vl_dcl       TYPE char10,
        vl_nf        TYPE j_1bnfnum9,
        vl_peso1     TYPE char20,
        vl_peso2     TYPE char20,
        vl_messagem  TYPE char100,
        vl_check     TYPE char1,
        vl_vagao     TYPE char7,
        vl_i         TYPE i,
        wl_cnpj      TYPE zlest0035-cnpj,
        wl_saldo     TYPE zlest0035-saldo,
        wl_docnum    TYPE zlest0035-docnum.

  SORT: ti_zlest0004 BY nr_fatura,
        ti_zlest0003 BY serie_despacho nr_despacho,
        ti_j_1bnfdoc BY nfenum nfnum bukrs branch,
        ti_j_1bnflin BY docnum,
        ti_vbfa      BY vbeln posnn,
        ti_lfa1      BY stcd1.


  CHECK NOT ti_vbfa[] IS INITIAL.

  tl_zlest0019[] = ti_zlest0019[].
  DELETE tl_zlest0019 WHERE: idinter NE 'L2',
                             tp_reg  NE '30'.

  LOOP AT tl_zlest0019 INTO sl_zlest0019.
    IF NOT sl_zlest0019-nfenum IS INITIAL.
      sl_aux-nfenum     = sl_zlest0019-nfenum.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = sl_zlest0019-nfnum
        IMPORTING
          output = sl_aux-nfenum.
    ENDIF.
    sl_aux-dcl        = sl_zlest0019-dcl.
    sl_aux-seriedcl   = sl_zlest0019-seriedcl.
    sl_aux-pesonf     = sl_zlest0019-pesonf.
    sl_aux-pesodvagao = sl_zlest0019-pesodvagao.
    COLLECT sl_aux INTO tl_aux.
    CLEAR: sl_zlest0019,
           sl_aux      .
  ENDLOOP.

* Loop - Transporte por Fatura
  LOOP AT ti_zlest0006 INTO wa_zlest0006.


    IF wa_zlest0006-emissao IS INITIAL.
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 space
                                 space
                                 space
                                 TEXT-028.
      "EXIT.
    ENDIF.


    v_tabix = sy-tabix.
    PERFORM zf_clear.
    CLEAR vl_check.

*----Verifica CNPJ
    CLEAR wa_lfa1.

    READ TABLE ti_lfa1 INTO wa_lfa1
      WITH KEY stcd1 = wa_zlest0006-cgc_all
      BINARY SEARCH.

    IF NOT sy-subrc IS INITIAL.
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 wa_zlest0006-cgc_all
                                 space
                                 space
                                 TEXT-010.
      " CONTINUE.
    ENDIF.
*----------

*----------Despachos
    READ TABLE ti_zlest0004 INTO wa_zlest0004
      WITH KEY nr_fatura = wa_zlest0006-nr_fatura
      BINARY SEARCH.

    vl_index = sy-tabix.

    IF NOT sy-subrc IS INITIAL.
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 space
                                 space
                                 space
                                 TEXT-008.
      CONTINUE.
    ENDIF.
    "--------------------

    "-------Loop Despachos - Ferroviario
    LOOP AT ti_zlest0004 INTO wa_zlest0004 FROM vl_index.
      "Caso haja erro na fatura anterior vl_check = 'X'
      IF NOT vl_check IS INITIAL.
        REFRESH: ti_itemdata,
                 t_save     .
        "EXIT.
      ENDIF.

      IF wa_zlest0004-nr_fatura NE wa_zlest0006-nr_fatura.
        EXIT.
      ENDIF.

*-------Verifica DCL e Série Fatura x Etapa 2
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zlest0004-nr_despacho
        IMPORTING
          output = vl_dcl.
      READ TABLE ti_zlest0019 INTO sl_zlest0019
        WITH KEY idinter  = 'L2'
                 tp_reg   = '20'
                 dcl      = vl_dcl
                 seriedcl = wa_zlest0004-serie_despacho
        BINARY SEARCH.

      IF NOT sy-subrc IS INITIAL.
        vl_check = 'X'.
*       Monta Erro
        PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                   wa_zlest0006-nr_fatura
                                   wa_zlest0004-serie_despacho
                                   wa_zlest0004-nr_despacho
                                   space
                                   space
                                   space
                                   TEXT-017.
        CONTINUE.
        "EXIT.
      ENDIF.
*-------------

*---------Verifica nº Vagão
      READ TABLE ti_zlest0005 INTO sl_zlest0005
        WITH KEY serie_despacho = wa_zlest0004-serie_despacho
                 nr_despacho    = wa_zlest0004-nr_despacho
        BINARY SEARCH.

      IF NOT sy-subrc IS INITIAL.
        vl_check = 'X'.
*       Monta Erro
        PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                   wa_zlest0006-nr_fatura
                                   wa_zlest0004-serie_despacho
                                   wa_zlest0004-nr_despacho
                                   space
                                   space
                                   space
                                   TEXT-018.
        CONTINUE.
        " EXIT.
      ENDIF.
*-----------------------

      READ TABLE ti_zlest0019 INTO sl_zlest0019
        WITH KEY idinter  = 'L2'
                 tp_reg   = '20'
                 dcl      = vl_dcl
                 seriedcl = wa_zlest0004-serie_despacho
        BINARY SEARCH.

      vl_vagao = sl_zlest0019-idvagao+03(07).
      vl_i     = strlen( vl_vagao ).
      SUBTRACT 1 FROM vl_i.
      IF vl_vagao+vl_i(01) EQ 'T'.
        vl_vagao+vl_i(01) = space.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_vagao
        IMPORTING
          output = vl_vagao.

      IF NOT sy-subrc          IS INITIAL OR
         sl_zlest0005-nr_vagao NE vl_vagao.
        vl_check = 'X'.
*       Monta Erro
        PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                   wa_zlest0006-nr_fatura
                                   wa_zlest0004-serie_despacho
                                   wa_zlest0004-nr_despacho
                                   space
                                   space
                                   space
                                   TEXT-019.
        "EXIT.
        "CONTINUE.
      ENDIF.

      IF sl_zlest0005-tipo_vagao NE sl_zlest0019-idvagao(03).
        vl_check = 'X'.
*       Monta Erro
        PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                   wa_zlest0006-nr_fatura
                                   wa_zlest0004-serie_despacho
                                   wa_zlest0004-nr_despacho
                                   space
                                   space
                                   space
                                   TEXT-020.
        "CONTINUE."EXIT.
      ENDIF.

*-------NF's Cliente
      CLEAR wa_zlest0003.
      READ TABLE ti_zlest0003 INTO wa_zlest0003
        WITH KEY serie_despacho = wa_zlest0004-serie_despacho
                 nr_despacho    = wa_zlest0004-nr_despacho
        BINARY SEARCH.

      vl_index = sy-tabix.

      IF NOT sy-subrc IS INITIAL.
        vl_check = 'X'.
*       Monta Erro
        PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                   wa_zlest0006-nr_fatura
                                   wa_zlest0004-serie_despacho
                                   wa_zlest0004-nr_despacho
                                   space
                                   space
                                   space
                                   TEXT-009.
        CONTINUE.
        "EXIT.
      ENDIF.
*---------

*-----Local de Negócios
      CLEAR wa_j_1bbranch.
      READ TABLE ti_j_1bbranch INTO wa_j_1bbranch
        WITH KEY stcd1 = wa_zlest0004-cgc_remetente
        BINARY SEARCH.

      IF NOT sy-subrc IS INITIAL.

        SELECT SINGLE lifnr
                      stcd1
          INTO wa_lfa1_2
          FROM lfa1
         WHERE stcd1 EQ wa_zlest0004-cgc_remetente.

        SELECT *
          FROM zlest0041
          INTO TABLE ti_zlest0041
         WHERE nr_nf_propria EQ wa_zlest0003-nr_nf
           AND cod_cliente   EQ wa_lfa1_2-lifnr.

        IF NOT sy-subrc IS INITIAL  .

          vl_check = 'X'.
*         Monta Erro
          PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                     wa_zlest0006-nr_fatura
                                     wa_zlest0004-serie_despacho
                                     wa_zlest0004-nr_despacho
                                     wa_zlest0004-cgc_remetente
                                     space
                                     space
                                     TEXT-010.
          CONTINUE.
          "EXIT.
        ENDIF.
      ENDIF.
*------------

      "---------Loop Notas fiscais de cliente
      LOOP AT ti_zlest0003 INTO wa_zlest0003 FROM vl_index.

        "IF NOT vl_check IS INITIAL.
        "  EXIT.
        "ENDIF.

        IF wa_zlest0003-serie_despacho NE wa_zlest0004-serie_despacho OR
           wa_zlest0003-nr_despacho    NE wa_zlest0004-nr_despacho.
          EXIT.
        ENDIF.

        "-----Quantidade NF
        READ TABLE ti_zlest0035 INTO sl_zlest0035
          WITH KEY nr_nf = wa_zlest0003-nr_nf  cnpj = wa_zlest0003-cgc_remetente werks = wa_zlest0003-branch
          BINARY SEARCH.

        IF NOT sy-subrc IS INITIAL.
*         Monta Erro
          PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                     wa_zlest0006-nr_fatura
                                     wa_zlest0004-serie_despacho
                                     wa_zlest0004-nr_despacho
                                     wa_zlest0004-cgc_remetente
                                     space
                                     wa_zlest0003-nr_nf
                                     TEXT-021.
          vl_check = 'X'.
          "EXIT.
          "CONTINUE.
        ENDIF.
        "-----Quantidade NF

*        READ TABLE ti_zlest0019 INTO sl_zlest0019
*        WITH KEY idinter  = 'L1'
*                 tp_reg   = '30'
*                 dcl      = vl_dcl
*                 seriedcl = wa_zlest0004-serie_despacho
*        BINARY SEARCH.

        "----ti_zlest0019_nfe
        READ TABLE ti_zlest0019_nfe INTO sl_zlest0019
          WITH KEY idinter  = 'L1'
                   tp_reg   = '30'
                   cnpjcliente = sl_zlest0035-cnpj
                   branch      = sl_zlest0035-werks
                   nfenum      = sl_zlest0035-nr_nf
          BINARY SEARCH.

        IF NOT sy-subrc IS INITIAL.
          READ TABLE ti_zlest0019_nf INTO sl_zlest0019
            WITH KEY idinter  = 'L1'
                     tp_reg   = '30'
                     cnpjcliente = sl_zlest0035-cnpj
                     branch      = sl_zlest0035-werks
                     nfnum       = sl_zlest0035-nr_nf+3(06)
            BINARY SEARCH.
        ENDIF.

        IF NOT sy-subrc IS INITIAL.
*         Monta Erro
          PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                     wa_zlest0006-nr_fatura
                                     wa_zlest0004-serie_despacho
                                     wa_zlest0004-nr_despacho
                                     wa_zlest0004-cgc_remetente
                                     space
                                     wa_zlest0003-nr_nf
                                     TEXT-022.
          vl_check = 'X'.
          "EXIT.
          CONTINUE.
        ENDIF.
        "------------------

        SORT tl_aux BY nfenum dcl seriedcl.

        "--------------Peso Confirmado Ferroviário (ZLEST0019)
        CLEAR: wl_saldo, wl_docnum.
        wl_cnpj = wa_zlest0004-cgc_remetente.
*        call function 'CONVERSION_EXIT_ALPHA_INPUT'
*          exporting
*            input  = wa_zlest0004-cgc_remetente
*          importing
*            output = wl_cnpj.
*
*         call function 'CONVERSION_EXIT_ALPHA_INPUT'
*          exporting
*            input  = wa_zlest0006-nr_nf_all
*          importing
*            output = wl_docnum.

        CALL FUNCTION 'Z_LES_BUSCA_SALDO_FERROVIARIO'
          EXPORTING
            nr_nf              = sl_zlest0035-nr_nf
            serie_nf           = sl_zlest0035-serie_nf
            cnpj               = wl_cnpj
*           docnum             = wl_docnum
          IMPORTING
            saldo              = wl_saldo
          EXCEPTIONS
            qtd_cheg_not_found = 1
            OTHERS             = 2.

        READ TABLE tl_aux INTO sl_aux
          WITH KEY nfenum = wa_zlest0003-nr_nf
                   dcl = vl_dcl
                   seriedcl = wa_zlest0004-serie_despacho
          BINARY SEARCH.

        IF sy-subrc IS INITIAL AND sl_aux-pesodvagao GT wl_saldo. "sl_zlest0035-saldo.
*          vl_peso1 = sl_zlest0035-saldo.
          vl_peso1 = wl_saldo.
          vl_peso2 = sl_aux-pesodvagao.
          CONDENSE: vl_peso1 NO-GAPS,
                    vl_peso2 NO-GAPS.
          CONCATENATE TEXT-023
                      wa_zlest0003-nr_nf
                      '.'
                      TEXT-024
                      vl_peso1
                      TEXT-025
                      vl_peso2
                 INTO vl_messagem SEPARATED BY space.
*         Monta Erro
          PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                     wa_zlest0006-nr_fatura
                                     wa_zlest0004-serie_despacho
                                     wa_zlest0004-nr_despacho
                                     wa_zlest0004-cgc_remetente
                                     space
                                     space
                                     vl_messagem.
          vl_check = 'X'.
          CONTINUE.
          "EXIT.
        ELSE.
          sl_zlest0035-saldo = wl_saldo - sl_aux-pesodvagao.
          APPEND sl_zlest0035 TO t_save.
        ENDIF.
        "------------
        "--------Cabeçalho NF
        CLEAR wa_j_1bnfdoc.
        READ TABLE ti_j_1bnfdoc INTO wa_j_1bnfdoc
          WITH KEY nfenum = wa_zlest0003-nr_nf
                   nfnum  = cn_zeros(6)
                   bukrs  = wa_j_1bbranch-bukrs
                   branch = wa_j_1bbranch-branch
          BINARY SEARCH.

        IF sy-subrc EQ 0.
          vl_subrc = sy-subrc.
          vl_index = sy-tabix.
        ELSE.
          CLEAR wa_j_1bnfdoc.
          READ TABLE ti_j_1bnfdoc INTO wa_j_1bnfdoc
            WITH KEY nfenum = cn_zeros
                     nfnum  = wa_zlest0003-nr_nf(6)
                     bukrs  = wa_j_1bbranch-bukrs
                     branch = wa_j_1bbranch-branch
            BINARY SEARCH.
          vl_subrc = sy-subrc.
          vl_index = sy-tabix.
        ENDIF.

        IF NOT vl_subrc IS INITIAL.

          SELECT SINGLE lifnr
                        stcd1
            INTO wa_lfa1_2
            FROM lfa1
           WHERE stcd1 EQ wa_zlest0004-cgc_remetente.


          SELECT SINGLE *
            FROM zlest0041
            INTO wa_zlest0041
           WHERE nr_nf_propria EQ wa_zlest0003-nr_nf
             AND cod_cliente   EQ wa_lfa1_2-lifnr.



          CLEAR wa_j_1bnfdoc.

          READ TABLE ti_j_1bnfdoc INTO wa_j_1bnfdoc
          WITH KEY nfenum = wa_zlest0003-nr_nf
                   nfnum  = cn_zeros(6)
                   branch = wa_zlest0041-centro_comprador
          BINARY SEARCH.

          vl_subrc = sy-subrc.
          vl_index = sy-tabix.

          IF NOT sy-subrc IS INITIAL.
*              Monta Erro
            PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                       wa_zlest0006-nr_fatura
                                       wa_zlest0004-serie_despacho
                                       wa_zlest0004-nr_despacho
                                       wa_zlest0004-cgc_remetente
                                       space
                                       space
                                       TEXT-004.
            vl_check = 'X'.
            "EXIT.
            CONTINUE.
          ENDIF.
        ENDIF.
        "-----------

        "------Verifica Cliente Arquivo x Cliente Doc
        "------Verifica Cliente Fatura  x Etapa 2
        PERFORM z_verifica_cliente USING wa_j_1bnfdoc
                                         sl_zlest0035
                                CHANGING vl_subrc.

        IF NOT vl_subrc IS INITIAL.
          vl_check = 'X'.
          EXIT.
        ENDIF.
        "------------

        "------Loop Nota Fiscal
        LOOP AT ti_j_1bnfdoc INTO wa_j_1bnfdoc FROM vl_index.
          "        LOOP AT ti_j_1bnfdoc INTO wa_j_1bnfdoc where nfenum eq wa_zlest0003-nr_nf.

          SELECT SINGLE lifnr
                        stcd1
            INTO wa_lfa1_2
            FROM lfa1
           WHERE stcd1 EQ wa_zlest0004-cgc_remetente.

          SELECT *
            FROM zlest0041
            INTO TABLE ti_zlest0041
           WHERE nr_nf_propria EQ wa_zlest0003-nr_nf
             AND cod_cliente   EQ wa_lfa1_2-lifnr.

          IF NOT sy-subrc IS INITIAL OR wa_j_1bnfdoc-nfenum NE wa_zlest0003-nr_nf.

            IF wa_j_1bnfdoc-nfe EQ c_x.
              IF wa_j_1bnfdoc-nfenum NE wa_zlest0003-nr_nf  OR
                 wa_j_1bnfdoc-bukrs  NE wa_j_1bbranch-bukrs OR
                 wa_j_1bnfdoc-branch NE wa_j_1bbranch-branch.
                EXIT.
              ENDIF.
            ELSE.
              IF wa_j_1bnfdoc-nfnum  NE wa_zlest0003-nr_nf(6) OR
                 wa_j_1bnfdoc-bukrs  NE wa_j_1bbranch-bukrs   OR
                 wa_j_1bnfdoc-branch NE wa_j_1bbranch-branch.
                EXIT.
              ENDIF.
            ENDIF.

          ENDIF.
*         Item NF
          CLEAR wa_j_1bnflin.
          READ TABLE ti_j_1bnflin INTO wa_j_1bnflin
            WITH KEY docnum = wa_j_1bnfdoc-docnum
            BINARY SEARCH.

          vl_index = sy-tabix.

          IF NOT sy-subrc IS INITIAL.
*           Monta Erro
            PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                       wa_zlest0006-nr_fatura
                                       wa_zlest0004-serie_despacho
                                       wa_zlest0004-nr_despacho
                                       wa_zlest0004-cgc_remetente
                                       space
                                       space
                                       TEXT-004.
            vl_check = 'X'.
            "EXIT.
            CONTINUE.
          ENDIF.
          "----Loop itens da nota
          LOOP AT ti_j_1bnflin INTO wa_j_1bnflin FROM vl_index.

            IF wa_j_1bnflin-docnum NE wa_j_1bnfdoc-docnum.
              EXIT.
            ENDIF.

            "-----------Remessa
            CLEAR wa_vbfa.
            READ TABLE ti_vbfa INTO wa_vbfa
              WITH KEY vbeln = wa_j_1bnflin-refkey(10)
                       posnn = wa_j_1bnflin-refitm
              BINARY SEARCH.

            vl_index = sy-tabix.

            IF NOT sy-subrc IS INITIAL.
*             Monta Erro
              PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                         wa_zlest0006-nr_fatura
                                         wa_zlest0004-serie_despacho
                                         wa_zlest0004-nr_despacho
                                         wa_zlest0004-cgc_remetente
                                         wa_j_1bnflin-refkey(10)
                                         space
                                         TEXT-011.
              vl_check = 'X'.
              "EXIT.
              CONTINUE.
            ENDIF.
            "---------------

            "------Loop Documentos de vendas
            LOOP AT ti_vbfa INTO wa_vbfa FROM vl_index.

              IF wa_j_1bnflin-refkey(10) NE wa_vbfa-vbeln OR
                 wa_j_1bnflin-refitm     NE wa_vbfa-posnn.
                EXIT.
              ENDIF.

              PERFORM zf_preenche_itens_bapi.

            ENDLOOP."Fim Loop - Documentos de vendas
          ENDLOOP."Fim Loop - itens da nota
        ENDLOOP."Fim Loop - Nota Fiscal
      ENDLOOP."Fim Loop - Notas fiscais de cliente
    ENDLOOP. "Fim Loop - Despachos - Ferroviario
*-----------

*      Não a necessidade de verificar se a remessa possui transporte (Clecio)
*      PERFORM zf_check_remessa_transporte.

    IF NOT ti_itemdata[] IS INITIAL AND vl_check IS INITIAL AND v_erro IS INITIAL.
      PERFORM zf_preenche_header_bapi.
      PERFORM zf_stage_data_bapi.
      IF NOT ti_stagedata[] IS INITIAL.
        PERFORM zf_executa_bapi.
      ENDIF.
    ENDIF.

  ENDLOOP. "Fim Loop - Transporte por Fatura

ENDFORM.                    " ZF_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_ITENS_BAPI
*&---------------------------------------------------------------------*
*       Preenche Estrutura BAPI-ITENS
*----------------------------------------------------------------------*
FORM zf_preenche_itens_bapi .

  wa_itemdata-delivery  = wa_vbfa-vbelv.
  wa_itemdata-itenerary = wa_vbfa-posnv.

  APPEND wa_itemdata TO ti_itemdata.

ENDFORM.                    " ZF_PREENCHE_ITENS_BAPI
*&---------------------------------------------------------------------*
*&      Form  ZF_CLEAR
*&---------------------------------------------------------------------*
*       Limpa Estruturas BAPI
*----------------------------------------------------------------------*
FORM zf_clear .

  CLEAR: ti_itemdata[],
         ti_stagedata[],
         ti_bdc[],
         wa_zlest0004,
         wa_zlest0003,
         t_save[].

ENDFORM.                    " ZF_CLEAR
*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_HEADER_BAPI
*&---------------------------------------------------------------------*
*       Preenche Estrutura BAPI-HEADER
*----------------------------------------------------------------------*
FORM zf_preenche_header_bapi .

  DATA : nr_nf TYPE c LENGTH 6.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zlest0006-nr_nf_all
    IMPORTING
      output = nr_nf.



  CLEAR wa_headerdata.

  wa_headerdata-shipment_type        = c_z003.

  IF wa_zlest0041-centro_comprador IS INITIAL.
    wa_headerdata-trans_plan_pt        = wa_j_1bbranch-branch.
    wa_headerdata-service_agent_id     = wa_lfa1-lifnr.
  ELSE.
    SELECT SINGLE lifnr
                  stcd1
      INTO wa_lfa1_2
      FROM lfa1
      WHERE stcd1 EQ wa_zlest0006-cgc_all.

    wa_headerdata-service_agent_id     = wa_lfa1_2-lifnr.
    wa_headerdata-trans_plan_pt        = wa_zlest0041-centro_comprador.
  ENDIF.

  wa_headerdata-service_level        = c_1.
  wa_headerdata-shipping_type        = c_02.
  wa_headerdata-external_id_1        = nr_nf."wa_zlest0006-nr_fatura.
  wa_headerdata-status_plan          = c_x.
  wa_headerdata-status_checkin       = c_x.
  wa_headerdata-status_load_start    = c_x.
  wa_headerdata-status_load_end      = c_x.
  wa_headerdata-status_compl         = c_x.
  wa_headerdata-status_shpmnt_start  = c_x.
  wa_headerdata-status_shpmnt_end    = c_x.

  wa_headerdata-tendering_carrier_track_id = wa_zlest0006-nr_fatura.
  wa_headerdata-time_travel          = c_30.
  wa_headerdata-time_total           = c_1.
  wa_headerdata-time_unit            = c_h.
  wa_headerdata-special_procedure_id = c_0001.
  wa_headerdata-shpmnt_cost_rel      = c_x.

ENDFORM.                    " ZF_PREENCHE_HEADER_BAPI
*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUTA_BAPI
*&---------------------------------------------------------------------*
*       BAPI Criação de Transporte
*----------------------------------------------------------------------*
FORM zf_executa_bapi.

  DATA: t_konh  TYPE TABLE OF konh,
        wa_konh TYPE konh.

  DATA: wk_vtfa             TYPE vtfa,
        wa_vfkp             TYPE vfkp,
        vl_msg              TYPE char100,
        wa_headerdata2      TYPE bapishipmentheader,
        wa_headerdataaction TYPE bapishipmentheaderaction,
        t_return            LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        vlr_dif             TYPE netwr_p,
        maxdif              TYPE setleaf-valfrom,
        maxdiv              TYPE netwr_p,
        vl_mensagem         TYPE string,
        vl_mensagem2        TYPE string,
        vlr_fatura          TYPE string,
        netwr               TYPE string,
        wdata               TYPE c LENGTH 10,
        vl_kbetr            TYPE konp-kbetr,
*---> 29/05/2023 - Migração S4 - JS
        "VL_VAKEY               TYPE KONH-VAKEY,
        vl_vakey            TYPE c LENGTH 100,
*<--- 29/05/2023 - Migração S4 - JS
        vl_itinerario       TYPE c LENGTH 10,
        vl_lzone            TYPE lfa1-lzone.

  CLEAR v_tknum.

*  SELECT SINGLE lzone
*    into vl_lzone
*    FROM lfa1
*   where stcd1 = WA_ZLEST0004-CGC_DEST.
*
*  vl_itinerario = wa_stagedata-org_point.
*    "Verificação se o itinerário esta cadastrado
*    "tipo de transporte + Fornecedor + itinerário + Tipo Contrato
*    CONCATENATE c_z003 wa_headerdata-service_agent_id  vl_itinerario vl_lzone INTO vl_vakey RESPECTING BLANKS.
*
*    SELECT SINGLE p~kbetr
*      INTO vl_kbetr
*      FROM konh AS h
*     INNER JOIN konp AS p ON p~knumh EQ h~knumh
*     WHERE h~kappl = 'F'
*       AND h~kvewe = 'A'
*       AND h~vakey = vl_vakey
*       AND datab  <= wa_zlest0006-emissao
*       AND datbi  >= wa_zlest0006-emissao .
*
*    IF ( NOT sy-subrc IS INITIAL ) OR  ( vl_kbetr <= 0 ) .
*
*      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
*                                 wa_zlest0006-nr_fatura
*                                 wa_zlest0004-serie_despacho
*                                 wa_zlest0004-nr_despacho
*                                 wa_zlest0004-cgc_remetente
*                                 space
*                                 space
*                                 text-029.
*      EXIT.
*
*    ENDIF.

  CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata = wa_headerdata
    IMPORTING
      transport  = v_tknum
    TABLES
      itemdata   = ti_itemdata
      stagedata  = ti_stagedata
      return     = ti_return.

  IF v_tknum IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    DELETE ti_return WHERE type NE c_e.

    CLEAR wa_zlest0008.
    LOOP AT ti_return INTO wa_return.

      PERFORM zf_get_max_log_ctrl USING sy-repid
                               CHANGING wa_zlest0008-cont
                                        wa_zlest0008-idctrl.

      wa_zlest0008-tcode   = sy-cprog.
      wa_zlest0008-msgtyp  = wa_return-type.
      wa_zlest0008-msgspra = sy-langu.
      wa_zlest0008-msgid   = wa_return-id.
      wa_zlest0008-msgnr   = wa_return-number.
      wa_zlest0008-msgv1   = wa_return-message.
      wa_zlest0008-data    = sy-datum.
      wa_zlest0008-hora    = sy-uzeit.
      wa_zlest0008-usuario = sy-uname.

      APPEND wa_zlest0008 TO ti_zlest0008.

*     Monta Erro
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 space
                                 space
                                 space
                                 wa_return-message.

    ENDLOOP.

  ELSE.

    CLEAR vl_msg.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    wa_zlest0006-status   = c_b.
    wa_zlest0006-nr_trans = v_tknum.
    CONCATENATE TEXT-026
                v_tknum
           INTO vl_msg SEPARATED BY space.


*    Monta Erro
    PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                               wa_zlest0006-nr_fatura
                               space
                               0
                               space
                               space
                               space
                               vl_msg.

    CONCATENATE wa_zlest0004-emissao+6(2) '.' wa_zlest0004-emissao+4(2) '.' wa_zlest0004-emissao(4) INTO  wdata.
    "CONCATENATE wa_zlest0006-emissao+6(2) '.' wa_zlest0006-emissao+4(2) '.' wa_zlest0006-emissao(4) INTO  wdata.

    PERFORM zf_bdc USING: 'X' 'SAPMV54A'   '0010',
                          ' ' 'BDC_CURSOR' 'VTTK-TKNUM',
                          ' ' 'BDC_OKCODE' '=UEBP',
                          ' ' 'VTTK-TKNUM' v_tknum,
                          ' ' 'BDC_CURSOR' 'VFKK-PRSDT',
                          ' ' 'BDC_OKCODE' '=UEBP',
                          ' ' 'VFKK-PRSDT'  wdata.

    PERFORM zf_bdc USING: 'X' 'SAPMV54A'   '0030',
                          ' ' 'BDC_CURSOR' 'VFKK-FKNUM',
                          ' ' 'BDC_OKCODE' '=SICH',
                          ' ' 'BDC_SUBSCR' 'SAPMV54A'.

    v_mode = c_n.

    CALL TRANSACTION c_vi01
       USING ti_bdc
       MODE   v_mode
       UPDATE c_s
       MESSAGES INTO ti_msg.

    COMMIT WORK AND WAIT.

    SELECT SINGLE *
      INTO  wk_vtfa
      FROM  vtfa
    WHERE vbelv   EQ v_tknum
      AND vbtyp_v EQ '8'
      AND vbtyp_n EQ 'a'.

    v_fknum = wk_vtfa-vbeln.

    SELECT SINGLE *
      FROM vfkp
      INTO wa_vfkp
     WHERE fknum = v_fknum.

    vlr_dif = abs( wa_zlest0006-vlr_fatura - wa_vfkp-netwr ).

    SELECT SINGLE valfrom
      INTO maxdif
      FROM setleaf
     WHERE setname EQ 'ZMAXDIF'.

    MOVE maxdif TO maxdiv.

    IF ( vlr_dif > maxdiv )  .

      vlr_fatura = wa_zlest0006-vlr_fatura.
      netwr      = wa_vfkp-netwr.

      CONCATENATE  'Fatura' ':' wa_zlest0006-nr_fatura ' ;' INTO vl_mensagem.
      CONCATENATE  'Valor Fatura: ' vlr_fatura '; Valor Custo: ' netwr INTO vl_mensagem2.

      MESSAGE i000(z01) WITH 'Valor da fatura diferente do valor '
                             'calculado no custo! '
                             vl_mensagem
                             vl_mensagem2.

    ENDIF.

    IF wk_vtfa IS NOT INITIAL .

      wa_zlest0006-nr_frete = v_fknum.
      MODIFY ti_zlest0006 FROM wa_zlest0006 INDEX v_tabix.
      CONCATENATE TEXT-027
                  v_fknum
             INTO vl_msg SEPARATED BY space.

*     Monta Erro
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 space
                                 space
                                 space
                                 vl_msg.

      IF NOT t_save[] IS INITIAL.
        MODIFY zlest0035 FROM TABLE t_save.
      ENDIF.

      CLEAR: wa_msg, v_fknum.
    ELSE.

      DELETE ti_msg WHERE msgtyp NE c_e.
      CLEAR wa_zlest0008.

      LOOP AT ti_msg INTO wa_msg.

        MESSAGE ID     wa_msg-msgid
                TYPE   wa_msg-msgtyp
                NUMBER wa_msg-msgnr
                WITH wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4
          INTO v_message.

        PERFORM zf_get_max_log_ctrl USING sy-repid
                                 CHANGING wa_zlest0008-cont
                                          wa_zlest0008-idctrl.

        wa_zlest0008-tcode   = sy-cprog.
        wa_zlest0008-msgtyp  = wa_msg-msgtyp.
        wa_zlest0008-msgspra = sy-langu.
        wa_zlest0008-msgid   = wa_msg-msgid.
        wa_zlest0008-msgnr   = wa_msg-msgnr.
        wa_zlest0008-msgv1   = v_message.
        wa_zlest0008-data    = sy-datum.
        wa_zlest0008-hora    = sy-uzeit.
        wa_zlest0008-usuario = sy-uname.

        APPEND wa_zlest0008 TO ti_zlest0008.

        CLEAR : wa_headerdataaction, wa_headerdata.

        CLEAR : wa_headerdata2.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_tknum
          IMPORTING
            output = wa_headerdata2-shipment_num.

        wa_headerdataaction-shipment_num = 'D'.
        wa_headerdataaction-service_agent_id = 'D'.

        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            headerdata       = wa_headerdata2
            headerdataaction = wa_headerdataaction
          TABLES
            return           = t_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.


*       Monta Erro
        PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                   wa_zlest0006-nr_fatura
                                   space
                                   0
                                   space
                                   space
                                   space
                                   wa_zlest0008-msgv1.
      ENDLOOP.

    ENDIF.

*    IF NOT t_save[] IS INITIAL.
*      MODIFY zlest0035 FROM TABLE t_save.
*    ENDIF.

  ENDIF.

ENDFORM.                    " ZF_EXECUTA_BAPI

*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZA_DADOS
*&---------------------------------------------------------------------*
*       Atualiza dados de Log e Processamento
*----------------------------------------------------------------------*
FORM zf_get_max_log_ctrl  USING p_chave
                       CHANGING s_cont
                                s_version.

  STATICS: lv_filename TYPE epsfilnam VALUE %_maxchar,
           lv_version  TYPE zidctrl,
           lv_vcont    TYPE numc10.
  DATA:    lv_chave     TYPE epsfilnam.

  CLEAR: s_cont,
         s_version.

  CONCATENATE p_chave '_' sy-tcode '_' sy-datum
         INTO lv_chave.

  IF lv_filename <> lv_chave.

    lv_filename = lv_chave.

    SELECT MAX( idctrl ) MAX( cont )
      INTO (lv_version, lv_vcont)
      FROM zlest0008
     WHERE filename = lv_filename
      GROUP BY idctrl.
    ENDSELECT.

    IF sy-subrc IS INITIAL.
      IF lv_vcont >= '9999999998'.
        ADD 1 TO lv_version.
        lv_vcont   = 0.
      ENDIF.
    ELSE.
      lv_version = 1.
      lv_vcont   = 0.
    ENDIF.

  ELSE.
    IF lv_vcont >= '9999999998'.
      ADD 1 TO lv_version.
      CLEAR lv_vcont.
    ENDIF.
  ENDIF.

  ADD 1 TO lv_vcont.

  s_cont    = lv_vcont.
  s_version = lv_version.

ENDFORM.                    " ZF_EXECUTA_BAPI

*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZA_DADOS
*&---------------------------------------------------------------------*
*       Atualiza dados de Log e Processamento
*----------------------------------------------------------------------*
FORM zf_atualiza_dados .

  IF NOT ti_zlest0008[] IS INITIAL.
    MODIFY zlest0008 FROM TABLE ti_zlest0008.
  ENDIF.

  DELETE ti_zlest0006 WHERE status NE c_b.

  IF NOT ti_zlest0006[] IS INITIAL.
    MODIFY zlest0006 FROM TABLE ti_zlest0006.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.                    " ZF_ATUALIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_EXECUCAO
*&---------------------------------------------------------------------*
*       Verifica se o Job esta em execução
*----------------------------------------------------------------------*
FORM zf_verifica_execucao.

  DATA tl_v_op TYPE TABLE OF v_op.

  SELECT *
    FROM v_op
    INTO TABLE tl_v_op
    WHERE status   EQ c_r AND
          progname EQ sy-repid.

  IF sy-subrc EQ 0 AND sy-dbcnt GT 1.
    v_erro = c_x.
  ENDIF.

ENDFORM.                    " ZF_VERIFICA_EXECUCAO

*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNBEGIN Tela Inicial
*      -->P_NAME     Campo
*      -->P_VALUE    Valor
*----------------------------------------------------------------------*
FORM zf_bdc USING p_dynbegin TYPE any
                  p_name     TYPE any
                  p_value    TYPE any.



  IF p_dynbegin EQ c_x.
    wa_bdc-program  = p_name.
    wa_bdc-dynpro   = p_value.
    wa_bdc-dynbegin = p_dynbegin.

    APPEND wa_bdc
      TO ti_bdc.
  ELSE.
    wa_bdc-fnam = p_name.
    wa_bdc-fval = p_value.

    APPEND wa_bdc
      TO ti_bdc.
  ENDIF.

  CLEAR wa_bdc.
ENDFORM.                    " ZF_BDC

*&---------------------------------------------------------------------*
*&      Form  ZF_STAGE_DATA_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_stage_data_bapi .

  DATA : ti_vttp  TYPE TABLE OF vttp WITH HEADER LINE,
         ti_vtts  TYPE TABLE OF vtts WITH HEADER LINE,
         ti_vbpa  TYPE TABLE OF vbpa WITH HEADER LINE,
         vl_lifnr TYPE lfa1-lifnr.

  REFRESH: ti_vtts     ,
           ti_vttp     ,
           ti_vbpa     ,
           ti_stagedata.


  IF trp EQ 'X' .

    SELECT *
      FROM vttp
      INTO TABLE ti_vttp
      FOR ALL ENTRIES IN ti_itemdata
    WHERE vbeln EQ ti_itemdata-delivery.

    IF NOT sy-subrc IS INITIAL.
*     Monta Erro
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 space
                                 space
                                 space
                                 TEXT-013.
      EXIT.
    ENDIF.

    SELECT *
      FROM vbpa
      INTO TABLE ti_vbpa
      FOR ALL ENTRIES IN ti_vttp
    WHERE vbeln EQ ti_vttp-vbeln
      AND lifnr NE space
      AND parvw EQ 'Z1'.

    IF NOT sy-subrc IS INITIAL.
*     Monta Erro
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 space
                                 space
                                 space
                                 TEXT-012.
      EXIT.
    ENDIF.

    SELECT *
      FROM vtts
      INTO TABLE ti_vtts
      FOR ALL ENTRIES IN ti_vttp
    WHERE tknum EQ ti_vttp-tknum.

    IF NOT sy-subrc IS INITIAL.
*     Monta Erro
      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                 wa_zlest0006-nr_fatura
                                 space
                                 0
                                 space
                                 space
                                 space
                                 TEXT-014.
      EXIT.
    ENDIF.

*  * Local de entrega
*    read table ti_vtpa with key parvw = 'LR'.
*    IF sy-subrc IS INITIAL.
*      wa_stagedata-dest_cust = ti_vtpa-kunnr.
*    ELSE.
*      CLEAR wa_stagedata-dest_point.
*    ENDIF.

*   Local de entrega - Fornecedor
    READ TABLE ti_vbpa INDEX 1.
    IF sy-subrc IS INITIAL.
      wa_stagedata-dest_suppl = ti_vbpa-lifnr.
    ELSE.
      CLEAR wa_stagedata-dest_suppl.
    ENDIF.

*   Determinação do local de origem
    READ TABLE ti_vtts INDEX 1.
    wa_stagedata-stage_cat  = c_1.
    wa_stagedata-stage_seq  = c_0001.
    wa_stagedata-org_point  = ti_vtts-knotz.

    IF wa_stagedata-org_point IS INITIAL.
      wa_stagedata-org_cust = ti_vtts-kunnz.
    ENDIF.
    IF wa_stagedata-org_cust IS INITIAL.
      wa_stagedata-org_suppl = ti_vtts-lifnz.
    ENDIF.

    APPEND wa_stagedata TO ti_stagedata.

  ELSE.
    SELECT SINGLE lifnr
      INTO vl_lifnr
      FROM lfa1
     WHERE stcd1 = wa_zlest0004-cgc_dest.

    wa_stagedata-stage_cat = c_1.
    wa_stagedata-stage_seq = c_0001.
    wa_stagedata-org_point = p_part.

    wa_stagedata-dest_suppl = vl_lifnr."wa_lfa1_2-lifnr ."ti_vtts-lifnz.

    APPEND wa_stagedata TO ti_stagedata.
  ENDIF.

ENDFORM.                    " ZF_STAGE_DATA_BAPI

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_REMESSA_TRANSPORTE
*&---------------------------------------------------------------------*
FORM zf_check_remessa_transporte .

  DATA: BEGIN OF lt_vbeln OCCURS 0,
          vbeln TYPE  vbeln_vl,
        END   OF lt_vbeln.

* Verifica se todos fornecimentos tem documento de transporte
  SELECT vbeln
    INTO TABLE lt_vbeln
    FROM vttp
     FOR ALL ENTRIES IN ti_itemdata
  WHERE vbeln EQ ti_itemdata-delivery.

  SORT lt_vbeln BY vbeln.

  LOOP AT ti_itemdata INTO wa_itemdata.
    READ TABLE lt_vbeln
      WITH KEY vbeln = wa_itemdata-delivery
      TRANSPORTING NO FIELDS
      BINARY SEARCH.
    IF sy-subrc <> 0.
      REFRESH ti_itemdata.
      EXIT.
    ENDIF.
  ENDLOOP.

  CLEAR wa_itemdata.

ENDFORM.                    " ZF_CHECK_REMESSA_TRANSPORTE

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                              Monta Erro                              *
*----------------------------------------------------------------------*
FORM z_monta_erro USING p_dacte TYPE n
                        p_fat   TYPE c
                        p_serie TYPE c
                        p_desp  TYPE n
                        p_cgc   TYPE c
                        p_ov    TYPE c
                        p_nf    TYPE c
                        p_msn   TYPE c.

  DATA sl_erro TYPE type_msn.

  sl_erro-dacte          = p_dacte.
  sl_erro-serie_despacho = p_serie.
  sl_erro-nr_despacho    = p_desp.
  sl_erro-cgc_remetente  = p_cgc.
  sl_erro-ov             = p_ov.
  sl_erro-nf             = p_nf.
  sl_erro-messagem       = p_msn.
  sl_erro-tp_msn         = 'E'.

  APPEND sl_erro TO ti_erros.
  v_erro = 'X'.

ENDFORM.                    " Z_MONTA_ERRO

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0006                                    *
*&---------------------------------------------------------------------*
*                          Faturas - Ferroviario                       *
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0006.

  REFRESH ti_zlest0006.

  SELECT *
    FROM zlest0006
    INTO TABLE ti_zlest0006
  WHERE status    EQ c_l
    AND nr_nf_all IN so_all
    AND cgc_all   EQ p_cnpj.

  SORT ti_zlest0006 BY nr_fatura.
  CHECK ti_zlest0006[] IS INITIAL.
* Monta Erro
  PERFORM z_monta_erro USING 0
                             space
                             space
                             0
                             space
                             space
                             space
                             TEXT-002.

ENDFORM.                    " Z_SELECIONA_ZLEST0006

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0004                                    *
*&---------------------------------------------------------------------*
*                        Despachos - Ferroviario                       *
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0004.

  DATA tl_zlest0006 TYPE TABLE OF zlest0006.

  REFRESH ti_zlest0004.

  CHECK NOT ti_zlest0006[] IS INITIAL.
  tl_zlest0006[] = ti_zlest0006[].
  DELETE ADJACENT DUPLICATES FROM tl_zlest0006 COMPARING nr_fatura.

  SELECT serie_despacho
         nr_despacho
         nr_fatura
         cgc_remetente
         cgc_dest
         emissao
    FROM zlest0004
    INTO TABLE ti_zlest0004
    FOR ALL ENTRIES IN tl_zlest0006
  WHERE nr_fatura     EQ tl_zlest0006-nr_fatura.

  SORT ti_zlest0004 BY serie_despacho nr_despacho.
  CHECK ti_zlest0004[] IS INITIAL.
* Monta Erro
  PERFORM z_monta_erro USING 0
                             space
                             space
                             0
                             space
                             space
                             space
                             TEXT-003.

ENDFORM.                    " Z_SELECIONA_ZLEST0004

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0003                                    *
*&---------------------------------------------------------------------*
*                              NF Clientes                             *
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0003.

  DATA: tl_zlest0004 TYPE TABLE OF ty_zlest0004,
        vl_branch    TYPE j_1bbranch-branch,
        vl_cont1     TYPE sy-index,
        t_branch     TYPE TABLE OF ty_j_1bbranch,
        st_branch    TYPE ty_j_1bbranch.

  REFRESH: ti_zlest0003, t_branch.

  CHECK NOT ti_zlest0004[] IS INITIAL.
  tl_zlest0004[] = ti_zlest0004[].
  DELETE ADJACENT DUPLICATES FROM tl_zlest0004
    COMPARING serie_despacho nr_despacho.

  SELECT z~serie_despacho
         z~nr_despacho
         z~nr_nf
         e~cgc_remetente
         d~cgc_cliente
    FROM zlest0003 AS z
    INNER JOIN zlest0004 AS e ON z~serie_despacho EQ e~serie_despacho AND z~nr_despacho EQ  e~nr_despacho
    INNER JOIN zlest0006 AS d ON d~nr_fatura EQ e~nr_fatura
    INTO TABLE ti_zlest0003_aux
    FOR ALL ENTRIES IN tl_zlest0004
  WHERE z~serie_despacho EQ tl_zlest0004-serie_despacho
    AND z~nr_despacho    EQ tl_zlest0004-nr_despacho.

  CLEAR : wa_zlest0003,wa_zlest0003_aux.

  LOOP AT ti_zlest0003_aux INTO wa_zlest0003_aux .

    wa_zlest0003-serie_despacho = wa_zlest0003_aux-serie_despacho.
    wa_zlest0003-nr_despacho    = wa_zlest0003_aux-nr_despacho   .
    wa_zlest0003-nr_nf          = wa_zlest0003_aux-nr_nf         .
    wa_zlest0003-cgc_remetente  = wa_zlest0003_aux-cgc_remetente .


*    SELECT SINGLE BRANCH
*      FROM J_1BBRANCH
*      INTO VL_BRANCH
*     WHERE STCD1 EQ WA_ZLEST0003_AUX-CGC_CLIENTE.


    " Conforme Chamado 85399 quando encontrar 2 branch ignorar o 0001

    REFRESH t_branch.

    SELECT bukrs branch stcd1
      FROM j_1bbranch
      INTO TABLE t_branch
     WHERE stcd1 EQ wa_zlest0003_aux-cgc_cliente.


    CLEAR: vl_cont1, st_branch.
    DESCRIBE TABLE t_branch LINES vl_cont1.

    IF vl_cont1 > 1.
      DELETE t_branch WHERE branch EQ '0001'.
    ENDIF.

    READ TABLE t_branch INTO st_branch WITH KEY stcd1 = wa_zlest0003_aux-cgc_cliente.


    " Fim Chamado 85399.


    "WA_ZLEST0003-BRANCH        = VL_BRANCH.

    wa_zlest0003-branch        = st_branch-branch.

    APPEND wa_zlest0003 TO ti_zlest0003.
    CLEAR : wa_zlest0003,wa_zlest0003_aux.
  ENDLOOP.


  SORT ti_zlest0003 BY serie_despacho nr_despacho.

  CHECK ti_zlest0003[] IS INITIAL.
* Monta Erro
  PERFORM z_monta_erro USING 0
                             space
                             space
                             0
                             space
                             space
                             space
                             TEXT-004.


ENDFORM.                    " Z_SELECIONA_ZLEST0003

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_J_1BBRANCH                                   *
*&---------------------------------------------------------------------*
*                          Local de Negocios                           *
*----------------------------------------------------------------------*
FORM z_seleciona_j_1bbranch.

  DATA :tl_stcd1 TYPE TABLE OF ty_zlesbranch,
        vl_cont1 TYPE sy-index.

  REFRESH: ti_j_1bbranch    ,
           ti_j_1bbranch_aux.

  DATA : vl_ok TYPE c LENGTH 1.

  CHECK NOT ti_zlest0004[] IS INITIAL.

  LOOP AT ti_zlest0004 INTO wa_zlest0004.
    CLEAR wa_j_1bbranch_aux.

    wa_j_1bbranch_aux-serie_despacho = wa_zlest0004-serie_despacho.
    wa_j_1bbranch_aux-nr_despacho    = wa_zlest0004-nr_despacho   .
    wa_j_1bbranch_aux-nr_fatura      = wa_zlest0004-nr_fatura     .
    wa_j_1bbranch_aux-stcd1          = wa_zlest0004-cgc_remetente .

    APPEND wa_j_1bbranch_aux TO ti_j_1bbranch_aux.

  ENDLOOP.

  "ti_j_1bbranch_aux[] = ti_zlest0004[].
  tl_stcd1[] = ti_j_1bbranch_aux[]. "ti_zlest0004[].

  SORT: ti_j_1bbranch_aux BY stcd1,
        tl_stcd1          BY stcd1.
  DELETE ADJACENT DUPLICATES FROM tl_stcd1 COMPARING stcd1.

  SELECT bukrs
         branch
         stcd1
    FROM j_1bbranch
    INTO TABLE ti_j_1bbranch
    FOR ALL ENTRIES IN tl_stcd1
  WHERE stcd1 EQ tl_stcd1-stcd1.

  " Conforme chamado quando encontrar 2 branch ignorar o 0001
  CLEAR vl_cont1.

  DESCRIBE TABLE ti_j_1bbranch LINES vl_cont1.

  IF vl_cont1 > 1.
    DELETE ti_j_1bbranch WHERE branch EQ '0001'.
  ENDIF.
  " Fim chamado


  SORT: ti_j_1bbranch     BY stcd1,
        ti_j_1bbranch_aux BY serie_despacho nr_despacho.

  CHECK ti_j_1bbranch[] IS INITIAL.

  vl_ok = 'N'.

  LOOP AT ti_zlest0003 INTO wa_zlest0003.

    READ TABLE ti_zlest0004 INTO wa_zlest0004
      WITH KEY serie_despacho = wa_zlest0003-serie_despacho nr_despacho = wa_zlest0003-nr_despacho BINARY SEARCH.

    SELECT SINGLE lifnr stcd1
      INTO wa_lfa1_2
      FROM lfa1
     WHERE stcd1 = wa_zlest0004-cgc_remetente.

    SELECT  *
      INTO TABLE ti_zlest0041
      FROM zlest0041
     WHERE nr_nf_propria = wa_zlest0003-nr_nf
       AND cod_cliente   = wa_lfa1_2-lifnr.

    IF sy-subrc IS INITIAL.
      vl_ok  = 'S'.
    ENDIF.

  ENDLOOP.

  IF vl_ok = 'N'.
*   Monta Erro
    PERFORM z_monta_erro USING 0
                               space
                               space
                               0
                               space
                               space
                               space
                               TEXT-005.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_J_1BBRANCH

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_J_1BNFDOC                                    *
*&---------------------------------------------------------------------*
*                             Cabeçalho NF's                           *
*----------------------------------------------------------------------*
FORM z_seleciona_j_1bnfdoc.

  REFRESH: ti_j_1bnfdoc_aux,
           ti_lfa1_aux     ,
           ti_lfa1         ,
           ti_j_1bnfdoc    .

* Monta dados para seleção J_1BNFDOC.
  LOOP AT ti_zlest0003 INTO wa_zlest0003.

    CLEAR: wa_zlest0004     ,
           wa_zlest0006     ,
           wa_j_1bbranch_aux,
           wa_j_1bnfdoc_aux ,
           wa_lfa1          .

    READ TABLE ti_zlest0004 INTO wa_zlest0004
      WITH KEY serie_despacho = wa_zlest0003-serie_despacho
               nr_despacho    = wa_zlest0003-nr_despacho
      BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    READ TABLE ti_j_1bbranch_aux INTO wa_j_1bbranch_aux
      WITH KEY serie_despacho = wa_zlest0003-serie_despacho
               nr_despacho    = wa_zlest0003-nr_despacho
      BINARY SEARCH.

    READ TABLE ti_j_1bbranch INTO wa_j_1bbranch
      WITH KEY stcd1 = wa_j_1bbranch_aux-stcd1
      BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      wa_j_1bnfdoc_aux-nfnum  = wa_zlest0003-nr_nf.
      wa_j_1bnfdoc_aux-nfenum = wa_zlest0003-nr_nf.
      wa_j_1bnfdoc_aux-bukrs  = wa_j_1bbranch-bukrs.
      wa_j_1bnfdoc_aux-branch = wa_j_1bbranch-branch.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_j_1bnfdoc_aux-nfenum
        IMPORTING
          output = wa_j_1bnfdoc_aux-nfenum.

      APPEND wa_j_1bnfdoc_aux TO ti_j_1bnfdoc_aux.

    ELSE.


      SELECT SINGLE lifnr stcd1
        INTO wa_lfa1_2
        FROM lfa1
       WHERE stcd1 = wa_zlest0004-cgc_remetente.

      SELECT SINGLE *
        INTO wa_zlest0041
        FROM zlest0041
       WHERE nr_nf_propria = wa_zlest0003-nr_nf
         AND cod_cliente   = wa_lfa1_2-lifnr.

      SELECT SINGLE bukrs
                    branch
                    stcd1
        INTO wa_j_1bbranch
        FROM j_1bbranch
       WHERE branch = wa_zlest0041-centro_comprador.

      wa_j_1bnfdoc_aux-nfnum  = wa_zlest0003-nr_nf.
      wa_j_1bnfdoc_aux-nfenum = wa_zlest0003-nr_nf.
      wa_j_1bnfdoc_aux-bukrs  = wa_j_1bbranch-bukrs.
      wa_j_1bnfdoc_aux-branch = wa_j_1bbranch-branch.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_j_1bnfdoc_aux-nfenum
        IMPORTING
          output = wa_j_1bnfdoc_aux-nfenum.

      APPEND wa_j_1bnfdoc_aux TO ti_j_1bnfdoc_aux.

    ENDIF.

    READ TABLE ti_zlest0006 INTO wa_zlest0006
      WITH KEY nr_fatura = wa_zlest0004-nr_fatura
      BINARY SEARCH.

    IF sy-subrc EQ 0.
      wa_lfa1-stcd1 = wa_zlest0006-cgc_all.
      APPEND wa_lfa1 TO ti_lfa1_aux.
    ENDIF.

  ENDLOOP.

  IF NOT ti_lfa1_aux[] IS INITIAL.
*   Dados Fornecedor
    PERFORM z_seleciona_lfa1.
  ELSE.
    EXIT.
  ENDIF.

  IF ti_j_1bnfdoc_aux[] IS INITIAL.
*   Monta Erro
    PERFORM z_monta_erro USING 0
                               space
                               space
                               0
                               space
                               space
                               space
                               TEXT-004.
    EXIT.
  ENDIF.

  SORT ti_j_1bnfdoc_aux BY nfnum nfenum bukrs branch.
  DELETE ADJACENT DUPLICATES FROM ti_j_1bnfdoc_aux
    COMPARING nfnum nfenum bukrs branch.

  SELECT docnum
         nfnum
         bukrs
         branch
         nfenum
         nfe
         parvw
         parid
    FROM j_1bnfdoc
    INTO TABLE ti_j_1bnfdoc
     FOR ALL ENTRIES IN ti_j_1bnfdoc_aux
  WHERE ( nfnum  EQ ti_j_1bnfdoc_aux-nfnum
     OR   nfenum EQ ti_j_1bnfdoc_aux-nfenum )
    AND   bukrs  EQ ti_j_1bnfdoc_aux-bukrs
    AND   branch EQ ti_j_1bnfdoc_aux-branch
    AND   direct = 2
    AND   cancel <> c_x.

  CHECK ti_j_1bnfdoc[] IS INITIAL.
* Monta Erro
  PERFORM z_monta_erro USING 0
                             space
                             space
                             0
                             space
                             space
                             space
                             TEXT-004.

ENDFORM.                    " Z_SELECIONA_J_1BNFDOC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LFA1                                         *
*&---------------------------------------------------------------------*
*                             Dados Fornecedor                         *
*----------------------------------------------------------------------*
FORM z_seleciona_lfa1.

  DATA tl_lfa1 TYPE TABLE OF ty_lfa1.

  REFRESH ti_lfa1.

  tl_lfa1[] = ti_lfa1_aux[].
  SORT: ti_lfa1_aux BY stcd1,
        tl_lfa1     BY stcd1.
  DELETE ADJACENT DUPLICATES FROM tl_lfa1 COMPARING stcd1.

  SELECT lifnr stcd1
    FROM lfa1
    INTO TABLE ti_lfa1
    FOR ALL ENTRIES IN tl_lfa1
  WHERE stcd1 EQ tl_lfa1-stcd1.

  CHECK ti_lfa1[] IS INITIAL.
* Monta Erro
  PERFORM z_monta_erro USING 0
                             space
                             space
                             0
                             space
                             space
                             space
                             TEXT-006.

ENDFORM.                    " Z_SELECIONA_LFA1

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_J_1BNFLIN                                    *
*&---------------------------------------------------------------------*
*                         Seleciona Itens NF's                         *
*----------------------------------------------------------------------*
FORM z_seleciona_j_1bnflin.

  REFRESH ti_j_1bnflin.

  CHECK NOT ti_j_1bnfdoc[] IS INITIAL.

  SELECT docnum
         refkey
         refitm
    FROM j_1bnflin
    INTO TABLE ti_j_1bnflin
    FOR ALL ENTRIES IN ti_j_1bnfdoc
  WHERE docnum EQ ti_j_1bnfdoc-docnum
    AND reftyp EQ c_bi.

  CHECK ti_j_1bnflin[] IS INITIAL.
* Monta Erro
  PERFORM z_monta_erro USING 0
                             space
                             space
                             0
                             space
                             space
                             space
                             TEXT-004.

ENDFORM.                    " Z_SELECIONA_J_1BNFLIN

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBFA                                         *
*&---------------------------------------------------------------------*
*                            Seleciona Remessas                        *
*----------------------------------------------------------------------*
FORM z_seleciona_vbfa.

  REFRESH ti_vbfa.

  CHECK NOT ti_j_1bnflin[] IS INITIAL.
  ti_j_1bnflin_aux[] = ti_j_1bnflin[].
  SORT ti_j_1bnflin_aux BY vbeln posnn.
  DELETE ADJACENT DUPLICATES FROM ti_j_1bnflin_aux
    COMPARING vbeln posnn.

  SELECT vbelv
         posnv
         vbeln
         posnn
         vbtyp_v
         vbtyp_n
    FROM vbfa
    INTO TABLE ti_vbfa
    FOR ALL ENTRIES IN ti_j_1bnflin_aux
    WHERE vbeln EQ ti_j_1bnflin_aux-vbeln AND
          posnn EQ ti_j_1bnflin_aux-posnn.

  DELETE ti_vbfa WHERE vbtyp_n NE c_m
                    OR vbtyp_v NE c_j.

  CHECK ti_vbfa[] IS INITIAL.
* Monta Erro
  PERFORM z_monta_erro USING 0
                             space
                             space
                             0
                             space
                             space
                             space
                             TEXT-007.

ENDFORM.                    " Z_SELECIONA_VBFA

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ERROS                                            *
*&---------------------------------------------------------------------*
*                                Exibe Erros                           *
*----------------------------------------------------------------------*
FORM z_exibe_erros.

  CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
    TABLES
      table    = ti_erros
    EXCEPTIONS
      fb_error = 1
      OTHERS   = 2.

ENDFORM.                    " Z_EXIBE_ERROS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSLEST005                                    *
*&---------------------------------------------------------------------*
*                          Vagões - Ferroviarios                       *
*----------------------------------------------------------------------*
FORM z_seleciona_zslest005.

  REFRESH ti_zlest0005.

  CHECK NOT ti_zlest0004[] IS INITIAL.

  SELECT *
    FROM zlest0005
    INTO TABLE ti_zlest0005
    FOR ALL ENTRIES IN ti_zlest0004
  WHERE  serie_despacho EQ ti_zlest0004-serie_despacho
    AND  nr_despacho    EQ ti_zlest0004-nr_despacho.

  SORT ti_zlest0005 BY serie_despacho
                       nr_despacho.

ENDFORM.                    " Z_SELECIONA_ZSLEST005

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0019
*&---------------------------------------------------------------------*
*                           Peso Confirmado
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0019.

  DATA: tl_zlest0019 TYPE TABLE OF zlest0019,
        sl_zlest0019 TYPE zlest0019,
        sl_zlest0004 TYPE ty_zlest0004.

  REFRESH ti_zlest0019.

  CHECK NOT ti_zlest0004[] IS INITIAL.

  LOOP AT ti_zlest0004 INTO sl_zlest0004.

    sl_zlest0019-seriedcl = sl_zlest0004-serie_despacho.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sl_zlest0004-nr_despacho
      IMPORTING
        output = sl_zlest0019-dcl.

    APPEND sl_zlest0019 TO tl_zlest0019.
    CLEAR: sl_zlest0004,
           sl_zlest0019.
  ENDLOOP.

  SELECT *
    FROM zlest0019
    INTO TABLE ti_zlest0019
    FOR ALL ENTRIES IN tl_zlest0019
  WHERE dcl      EQ tl_zlest0019-dcl
    AND seriedcl EQ tl_zlest0019-seriedcl.

  SORT ti_zlest0019 BY idinter
                       tp_reg
                       dcl
                       seriedcl.

  CHECK NOT ti_zlest0019[] IS INITIAL.
  tl_zlest0019[] = ti_zlest0019[].
  SORT tl_zlest0019 BY cnpjcliente ASCENDING
                       bukrs       ASCENDING
                       branch      ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_zlest0019 COMPARING cnpjcliente
                                                         bukrs
                                                         branch.
  SELECT *
    FROM zlest0019
    INTO TABLE ti_zlest0019_nfe
    FOR ALL ENTRIES IN ti_zlest0019
  WHERE  cnpjcliente EQ ti_zlest0019-cnpjcliente
    AND  bukrs       EQ ti_zlest0019-bukrs
    AND  branch      EQ ti_zlest0019-branch.

  SORT ti_zlest0019_nfe BY idinter     ASCENDING
                           tp_reg      ASCENDING
                           cnpjcliente ASCENDING
                           bukrs       ASCENDING
                           branch      ASCENDING
                           nfenum      ASCENDING.
  ti_zlest0019_nf[] = ti_zlest0019_nfe[].
  SORT ti_zlest0019_nf BY  idinter     ASCENDING
                           tp_reg      ASCENDING
                           cnpjcliente ASCENDING
                           bukrs       ASCENDING
                           branch      ASCENDING
                           nfnum       ASCENDING.

ENDFORM.                    " Z_SELECIONA_ZLEST0019

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_CLIENTE                                       *
*&---------------------------------------------------------------------*
*               Verifica Cliente Arquivo x Cliente Doc                 *
*----------------------------------------------------------------------*
FORM z_verifica_cliente USING p_j_1bnfdoc TYPE ty_j_1bnfdoc
                              p_zlest0035 TYPE zlest0035
                     CHANGING p_subrc     TYPE sy-subrc.

  DATA: vl_stcd1     TYPE kna1-stcd1,
        vl_dcl       TYPE char10,
        sl_zlest0019 TYPE zlest0019.

  CLEAR p_subrc.


  SELECT SINGLE lifnr
                stcd1
     INTO wa_lfa1_2
     FROM lfa1
    WHERE stcd1 EQ p_zlest0035-cnpj.

  SELECT SINGLE *
    FROM zlest0041
    INTO wa_zlest0041
   WHERE nr_nf_propria EQ p_zlest0035-nr_nf
     AND cod_cliente   EQ wa_lfa1_2-lifnr.

  IF NOT sy-subrc IS INITIAL.

    SELECT SINGLE stcd1
      FROM j_1bbranch
      INTO vl_stcd1
    WHERE  bukrs  EQ p_j_1bnfdoc-bukrs
      AND  branch EQ p_j_1bnfdoc-parid+06(04).

    IF sy-subrc IS INITIAL.

      CASE p_j_1bnfdoc-parvw.
        WHEN 'LF'.
          SELECT SINGLE stcd1
            FROM lfa1
            INTO vl_stcd1
          WHERE  lifnr EQ p_j_1bnfdoc-parid.
        WHEN 'AG'.
          SELECT SINGLE stcd1
            FROM kna1
            INTO vl_stcd1
          WHERE  kunnr EQ p_j_1bnfdoc-parid.
        WHEN OTHERS.
          SELECT SINGLE stcd1
            FROM j_1bbranch
            INTO vl_stcd1
          WHERE  bukrs  EQ p_j_1bnfdoc-bukrs
            AND  branch EQ p_j_1bnfdoc-parid+06(04).
      ENDCASE.

      IF wa_zlest0004-cgc_remetente NE vl_stcd1.

        p_subrc = 1.
*       Monta Erro
        PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
                                   wa_zlest0006-nr_fatura
                                   wa_zlest0004-serie_despacho
                                   wa_zlest0004-nr_despacho
                                   wa_zlest0004-cgc_remetente
                                   space
                                   space
                                   TEXT-015.
      ENDIF.
    ENDIF.
*  ELSE.

*    SELECT SINGLE lifnr
*                   stcd1
*      INTO wa_lfa1
*      FROM lfa1
*     WHERE stcd1 EQ p_zlest0035-cnpj.
*
*    SELECT SINGLE *
*      FROM zlest0041
*      INTO wa_zlest0041
*     WHERE nr_nf_propria EQ p_zlest0035-nr_nf
*       AND cod_cliente   EQ wa_lfa1-lifnr.
*
*    IF NOT sy-subrc IS INITIAL.

*    IF wa_lfa1-stcd1 NE vl_stcd1.
*      PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
*              wa_zlest0006-nr_fatura
*              wa_zlest0004-serie_despacho
*              wa_zlest0004-nr_despacho
*              wa_zlest0004-cgc_remetente
*              space
*              space
*              text-015.
*
*    ENDIF.

*    ENDIF.



  ENDIF.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wa_zlest0004-nr_despacho
*    IMPORTING
*      output = vl_dcl.
*
*  READ TABLE ti_zlest0019 INTO sl_zlest0019
*    WITH KEY idinter  = 'L2'
*             tp_reg   = '10'
*             dcl      = vl_dcl
*             seriedcl = wa_zlest0004-serie_despacho
*    BINARY SEARCH.

*  READ TABLE ti_zlest0019_nfe INTO sl_zlest0019
*          WITH KEY idinter  = 'L2'
*                   tp_reg   = '10'
*                   cnpjcliente = p_zlest0035-cnpj
*                   branch      = p_zlest0035-werks
*                   nfenum      = p_zlest0035-nr_nf
*          BINARY SEARCH.
*
*  IF NOT sy-subrc IS INITIAL.
*    READ TABLE ti_zlest0019_nf INTO sl_zlest0019
*      WITH KEY idinter  = 'L2'
*               tp_reg   = '10'
*               cnpjcliente = p_zlest0035-cnpj
*               branch      = p_zlest0035-werks
*               nfnum       = p_zlest0035-nr_nf+3(06)
*      BINARY SEARCH.
*  ENDIF.
*
*  IF wa_zlest0006-cgc_cliente NE sl_zlest0019-cnpjferro.
*    p_subrc = 1.
**   Monta Erro
*    PERFORM z_monta_erro USING wa_zlest0006-nr_nf_all
*                               wa_zlest0006-nr_fatura
*                               wa_zlest0004-serie_despacho
*                               wa_zlest0004-nr_despacho
*                               wa_zlest0004-cgc_remetente
*                               space
*                               space
*                               text-016.
*  ENDIF.

ENDFORM.                    " Z_VERIFICA_CLIENTE

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0035                                    *
*&---------------------------------------------------------------------*
*                        Controle Saldo Ferroviário                    *
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0035.

  REFRESH ti_zlest0035.

  CHECK NOT ti_zlest0003[] IS INITIAL.

  SELECT *
    FROM zlest0035
    INTO TABLE ti_zlest0035
    FOR ALL ENTRIES IN ti_zlest0003
  WHERE nr_nf EQ ti_zlest0003-nr_nf
    AND cnpj  EQ ti_zlest0003-cgc_remetente
    AND werks EQ ti_zlest0003-branch.

  SORT ti_zlest0035 BY nr_nf cnpj werks ASCENDING.

ENDFORM.                    " Z_SELECIONA_ZLEST0035
*&---------------------------------------------------------------------*
*&      Form  HIDE_OPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hide_options .

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'TRP'.
        IF trp EQ 'X' .
          screen-active = 0.
          MODIFY SCREEN.
        ELSE.
          screen-active = 1.
          MODIFY SCREEN.
        ENDIF.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    " HIDE_OPTIONS
