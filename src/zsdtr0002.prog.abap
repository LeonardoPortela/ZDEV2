**************************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                               *
* Data desenv ...: 07.05.2015                                                        *
* Objetivo    ...: Notas Fiscais Exportação X Entrada – Carga de Informações – PwSATI*
* Transação   ...: ZSDT0094                                                          *
**************************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request                  *
**************************************************************************************
* 12.05.2015  Welgem Barbosa   Criação      09:43          DEVK946271                *
**************************************************************************************


REPORT  zsdtr0002.

TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*&      TABELAS UTILIZADAS
*&---------------------------------------------------------------------*
TABLES: zdde, zdde_aplicacao, zdoc_exp, vbfa, j_1bnflin, j_1bnfdoc, zdoc_nf_produtor.


*&---------------------------------------------------------------------*
*&      ESTRUTURA DE SAIDA PARA ALV
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_saida,
    docnum   TYPE j_1bnflin-docnum,
    itmnum   TYPE j_1bnflin-itmnum,
    docdat   TYPE zlft_exp_conv_20-data_docnum_s, "J_1BNFDOC-DOCDAT,
    cfop     TYPE j_1bnflin-cfop,
    vbeln_p  TYPE zdoc_nf_produtor-docnum_prod,
    itmnum_j TYPE j_1bnflin-itmnum,
    pstdat   TYPE zlft_exp_conv_20-data_docnum_e,
  END OF ty_saida,


*&---------------------------------------------------------------------*
*&      TABELA PARA CONVERSÃO DE TIPO DE DADOS
*&---------------------------------------------------------------------*
  BEGIN OF ty_vbfa,

    vbelv    TYPE vbfa-vbelv,
    vbeln    TYPE vbfa-vbeln,
    vbeln_fk TYPE j_1bnflin-refkey,

  END OF ty_vbfa,


*&---------------------------------------------------------------------*
*&      TABELA PARA QUANTIDADE
*&---------------------------------------------------------------------*
* Sonda RF-4791902 ----------------------------------------------------
  BEGIN OF ty_reme,

    docnum        TYPE j_1bdocnum,
    itmnum        TYPE j_1bitmnum,
    nr_quantidade TYPE j_1bnetqty,

  END OF ty_reme.
* Sonda RF-4791902 ----------------------------------------------------
*&---------------------------------------------------------------------*
*&      TABELAS INTERNAS
*&---------------------------------------------------------------------*
DATA: it_saida            TYPE TABLE OF ty_saida,
      it_zdde             TYPE TABLE OF zdde,
      it_zsdt0170         TYPE TABLE OF zsdt0170,
      it_zdde_aplicacao   TYPE TABLE OF zdde_aplicacao,
      it_zdoc_exp         TYPE TABLE OF zdoc_exp,
      it_vbfa             TYPE TABLE OF ty_vbfa,
      it_e_j_1bnflin      TYPE TABLE OF j_1bnflin,
      it_s_j_1bnflin      TYPE TABLE OF j_1bnflin,
      it_zdoc_nf_produtor TYPE TABLE OF zdoc_nf_produtor,
      it_e_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
      it_s_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
      it_estru_saida      TYPE TABLE OF zlft_exp_conv_20,
* Sonda RF-4791902 ----------------------------------------------------
      it_estru_reme       TYPE TABLE OF ty_reme,
* Sonda RF-4791902 ----------------------------------------------------
*&---------------------------------------------------------------------*
*&      WORK AREAS
*&---------------------------------------------------------------------*
* Sonda RF-4791902 ----------------------------------------------------
      wa_estru_rem        TYPE ty_reme,
* Sonda RF-4791902 ----------------------------------------------------
      wa_saida            TYPE ty_saida,
      wa_zdde             TYPE zdde,
      wa_zsdt0170         TYPE zsdt0170,
      wa_zdde_aplicacao   TYPE zdde_aplicacao,
      wa_zdoc_exp         TYPE zdoc_exp,
      wa_vbfa             TYPE ty_vbfa,
      wa_e_j_1bnflin      TYPE j_1bnflin,
      wa_s_j_1bnflin      TYPE j_1bnflin,
      wa_zdoc_nf_produtor TYPE zdoc_nf_produtor,
      wa_e_j_1bnfdoc      TYPE j_1bnfdoc,
      wa_s_j_1bnfdoc      TYPE j_1bnfdoc,
      wa_estru_saida      TYPE zlft_exp_conv_20,

      doc_s               TYPE c,
      doc_e               TYPE c,



*&---------------------------------------------------------------------*
*&      ESTRUTURA DA ALV
*&---------------------------------------------------------------------*
      it_fcat             TYPE slis_t_fieldcat_alv,
      wa_fcat             TYPE slis_fieldcat_alv,
      wa_layout           TYPE slis_layout_alv
      .

*&---------------------------------------------------------------------*
*&      CRIANDO UM FIELD SYMBOLS PARA CONVERTER AS INFORMAÇÕES
*&---------------------------------------------------------------------*
FIELD-SYMBOLS <fs_vbfa>  TYPE ty_vbfa.


*&---------------------------------------------------------------------*
*&      OPÇÃO DE BUSCA POR AVERBAÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: busca FOR zdde-dt_averbacao.
SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&      DECLARANDO OS PERFORM PARA CHAMADA DA FUNÇÃO
*&---------------------------------------------------------------------*
PERFORM: seleciona_dados, imprime_dados, armazena_dados, alv, feed_alv.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_dados .

*---------------------------------------------------------------*
* Busca Registros DDE/RE
*---------------------------------------------------------------*

  SELECT * FROM zdde INTO TABLE it_zdde
           WHERE dt_averbacao IN busca.

  IF it_zdde[] IS NOT INITIAL.

    SELECT * FROM zdde_aplicacao INTO TABLE it_zdde_aplicacao
    FOR ALL ENTRIES IN it_zdde
    WHERE id_dde EQ it_zdde-id_dde.

    IF it_zdde_aplicacao[] IS NOT INITIAL.
      SELECT * FROM zdoc_exp AS a INTO TABLE it_zdoc_exp
      FOR ALL ENTRIES IN it_zdde_aplicacao
      WHERE id_registro_expo EQ it_zdde_aplicacao-id_registro_expo
        AND NOT EXISTS ( SELECT *
                           FROM zdoc_exp_recusa AS b
                          WHERE b~id_doc_exp = a~id_doc_exp ).
    ENDIF.
  ENDIF.

*---------------------------------------------------------------*
* Busca Registros DU-e
*---------------------------------------------------------------*
  CLEAR: it_zsdt0170[].
  SELECT *
    FROM zsdt0170 AS a INTO TABLE it_zsdt0170
   WHERE situacao_due EQ '70'	"AVERBADA
     AND dt_situacao  IN busca
     AND performance  EQ abap_false
     AND EXISTS ( SELECT *
                   FROM zdoc_exp AS b
                  WHERE b~id_due = a~id_due ).

  IF it_zsdt0170[] IS NOT INITIAL.
    SELECT *
      FROM zdoc_exp AS a APPENDING TABLE it_zdoc_exp
       FOR ALL ENTRIES IN it_zsdt0170
     WHERE id_due EQ it_zsdt0170-id_due
       AND NOT EXISTS ( SELECT *
                          FROM zdoc_exp_recusa AS b
                         WHERE b~id_doc_exp = a~id_doc_exp ).
  ENDIF.

  CHECK NOT it_zdoc_exp IS INITIAL.

  SELECT a~vbelv a~vbeln
    FROM vbfa AS a INTO TABLE it_vbfa
     FOR ALL ENTRIES IN it_zdoc_exp
   WHERE a~vbelv    EQ it_zdoc_exp-vbeln
     AND a~vbtyp_n  EQ 'M'
     AND a~vbtyp_v  EQ 'J'
     AND NOT EXISTS ( SELECT *
                        FROM vbfa AS b
                       WHERE b~vbelv   EQ a~vbeln
                         AND b~vbtyp_n EQ 'N' "estorno
                     ).

*&      LOOP PARA INSERIR OS VALORES DE VBELN CHAR10 PARA VBELN_FK CHAR35
  LOOP AT it_vbfa ASSIGNING <fs_vbfa>.
    <fs_vbfa>-vbeln_fk = <fs_vbfa>-vbeln.
  ENDLOOP.

  CHECK NOT it_vbfa IS INITIAL.
  SELECT *  FROM j_1bnflin INTO TABLE it_s_j_1bnflin
  FOR ALL ENTRIES IN it_vbfa             WHERE refkey           EQ it_vbfa-vbeln_fk.

  CHECK NOT it_s_j_1bnflin IS INITIAL.
  SELECT *  FROM j_1bnfdoc INTO TABLE it_s_j_1bnfdoc
  FOR ALL ENTRIES IN it_s_j_1bnflin        WHERE docnum           EQ it_s_j_1bnflin-docnum.

  SORT it_s_j_1bnfdoc BY docnum ASCENDING.

  SELECT * FROM zdoc_nf_produtor INTO TABLE it_zdoc_nf_produtor
  FOR ALL ENTRIES IN it_zdoc_exp         WHERE vbeln            EQ it_zdoc_exp-vbeln.

  CHECK NOT it_zdoc_nf_produtor IS INITIAL.
  SELECT *  FROM j_1bnfdoc INTO TABLE it_e_j_1bnfdoc
  FOR ALL ENTRIES IN it_zdoc_nf_produtor WHERE docnum           EQ it_zdoc_nf_produtor-docnum_prod.

  CHECK NOT it_e_j_1bnfdoc IS INITIAL.
  SELECT *  FROM j_1bnflin INTO TABLE it_e_j_1bnflin
  FOR ALL ENTRIES IN it_e_j_1bnfdoc             WHERE docnum    EQ it_e_j_1bnfdoc-docnum.


  SELECT docnum_e docnum_s FROM zlft_exp_conv_20 INTO TABLE it_estru_saida.

* Sonda RF-4791902 ----------------------------------------------------
  SELECT docnum itmnum nr_quantidade FROM znom_reme_notas INTO TABLE it_estru_reme.
* Sonda RF-4791902 ----------------------------------------------------

ENDFORM.                    "SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       IMPRIME OS DADOS NA ALV SENDO 1 SAIDA PARA N ENTRADAS
*----------------------------------------------------------------------*
FORM imprime_dados.
  LOOP AT it_e_j_1bnfdoc INTO wa_e_j_1bnfdoc.

    CLEAR: wa_zdoc_nf_produtor, wa_zdoc_exp.

    LOOP AT it_zdoc_nf_produtor INTO wa_zdoc_nf_produtor WHERE docnum_prod = wa_e_j_1bnfdoc-docnum.

      READ TABLE it_zdoc_exp         INTO wa_zdoc_exp         WITH KEY vbeln       = wa_zdoc_nf_produtor-vbeln.

      LOOP AT it_vbfa  INTO wa_vbfa WHERE vbelv = wa_zdoc_exp-vbeln.

        CLEAR: wa_saida, wa_e_j_1bnflin, wa_s_j_1bnflin, wa_s_j_1bnfdoc.

        READ TABLE it_e_j_1bnflin      INTO wa_e_j_1bnflin      WITH KEY docnum      = wa_zdoc_nf_produtor-docnum_prod.
        READ TABLE it_s_j_1bnflin      INTO wa_s_j_1bnflin      WITH KEY refkey      = wa_vbfa-vbeln_fk.
        READ TABLE it_s_j_1bnfdoc      INTO wa_s_j_1bnfdoc      WITH KEY docnum      = wa_s_j_1bnflin-docnum.

        wa_saida-docnum   =  wa_s_j_1bnflin-docnum.               "documento de saida
        wa_saida-itmnum   =  wa_s_j_1bnflin-itmnum.               "ID do item de saida
        PERFORM formata_data   USING wa_s_j_1bnfdoc-docdat
                            CHANGING wa_saida-docdat.

        "WA_SAIDA-DOCDAT   =  WA_S_J_1BNFDOC-DOCDAT.               "data documento de saida
        wa_saida-cfop     =  wa_s_j_1bnflin-cfop.                 "CFOP item documento de saida
        wa_saida-vbeln_p  =  wa_zdoc_nf_produtor-docnum_prod.   "documento de Entrada
        wa_saida-itmnum_j =  wa_e_j_1bnflin-itmnum.               "ID do item de Entrada

        PERFORM formata_data   USING wa_e_j_1bnfdoc-pstdat
                            CHANGING wa_saida-pstdat.

        "WA_SAIDA-PSTDAT   =  WA_E_J_1BNFDOC-PSTDAT.               "data documento de entrada


        APPEND wa_saida TO it_saida.

      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    "IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv .
  PERFORM alv_preenche_cat USING:
" Sequencia da ALV para impressão

        "Campo"           "Descrição"          "qtd de string"   "no-0"
        'DOCNUM'          'DOCNUM_SAIDA'             '20'         ''      '' '',
        'ITMNUM'          'ID_ITEM_DOCNUM_SAIDA'     '20'         ''      '' '',
        'DOCDAT'          'DATA_DOCNUM_SAIDA'        '20'         ''      '' '',
        'CFOP'            'CFOP_ITEM_DOCNUM_SAIDA'   '20'         ''      '' '',
        'VBELN_P'         'DOCNUM_ENTRADA'           '20'         ''      '' '',
        'ITMNUM_J'        'ID_ITEM_DOCNUM_ENTRADA'   '20'         ''      '' '',
        'PSTDAT'          'DATA_DOCNUM_ENTRADA'      '20'         ''      '' ''.



ENDFORM.                    " ALV

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_CAMPO)  text
*      -->VALUE(P_DESC)   text
*      -->VALUE(P_TAM)    text
*      -->VALUE(P_ZERO)   text
*      -->VALUE(P_HOT)    text
*      -->VALUE(P_SUM)    text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING    VALUE(p_campo)
                                VALUE(p_desc)
                                VALUE(p_tam)
                                VALUE(p_zero)
                                VALUE(p_hot)
                                VALUE(p_sum).
  wa_fcat-fieldname  = p_campo.
  wa_fcat-seltext_m  = p_desc.
  wa_fcat-outputlen  = p_tam.
  wa_fcat-hotspot    = p_hot.
  wa_fcat-no_zero    = p_zero.
  wa_fcat-do_sum     = p_sum.

  APPEND wa_fcat TO it_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  FEED_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM feed_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat = it_fcat
    TABLES
      t_outtab    = it_saida.
ENDFORM.                    " TELA
*&---------------------------------------------------------------------*
*&      Form  ARMAZENA_DADOS
*&---------------------------------------------------------------------*
*       ARMAZENA OS DADOS PARA A TABELA ZLFT_EXP_CONV_20 SEMPRE QUE
*       FOR REALIZADO A PESQUISA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM armazena_dados .



  LOOP AT it_saida INTO wa_saida.

    " INSERE A INFORMAÇÃO
*    READ TABLE IT_ESTRU_SAIDA INTO WA_ESTRU_SAIDA WITH KEY DOCNUM_S = WA_SAIDA-DOCNUM.
*    DOC_S = SY-SUBRC.

    READ TABLE it_estru_saida WITH KEY docnum_s = wa_saida-vbeln_p
                                       docnum_e = wa_saida-docnum TRANSPORTING NO FIELDS.
    doc_e = sy-subrc.

    "    IF ( DOC_S NE 0 ) OR ( DOC_E NE 0 ).
    IF doc_e NE 0.
* Sonda RF-4791902 ----------------------------------------------------
      READ TABLE it_estru_reme into wa_estru_rem WITH KEY docnum = wa_saida-vbeln_p
                                                          itmnum = wa_saida-itmnum_j.

      IF sy-subrc IS INITIAL.
        wa_estru_saida-nr_quantidade   =      wa_estru_rem-nr_quantidade.
      ENDIF.
* Sonda RF-4791902 ----------------------------------------------------
      wa_estru_saida-docnum_s          =      wa_saida-docnum  .
      wa_estru_saida-id_item_docnum_s  =      wa_saida-itmnum  .
      wa_estru_saida-data_docnum_s     =      wa_saida-docdat  .
      wa_estru_saida-cfop_docnum_s     =      wa_saida-cfop    .
      wa_estru_saida-docnum_e          =      wa_saida-vbeln_p .
      wa_estru_saida-id_item_docnum_e  =      wa_saida-itmnum_j.
      wa_estru_saida-data_docnum_e     =      wa_saida-pstdat  .
      wa_estru_saida-usuario           =      sy-uname.
      wa_estru_saida-datatual          =      sy-datlo.
      wa_estru_saida-horatual          =      sy-uzeit.


      INSERT INTO zlft_exp_conv_20 VALUES wa_estru_saida.
      COMMIT WORK.
      CLEAR: wa_saida, wa_estru_saida.

    ELSE.
* Sonda RF-4791902 ----------------------------------------------------
      READ TABLE it_estru_reme into wa_estru_rem WITH KEY docnum = wa_saida-vbeln_p
                                                          itmnum = wa_saida-itmnum_j.

      IF sy-subrc IS INITIAL.
        wa_estru_saida-nr_quantidade   =      wa_estru_rem-nr_quantidade.
      ENDIF.
* Sonda RF-4791902 ----------------------------------------------------
      wa_estru_saida-docnum_s          =      wa_saida-docnum  .
      wa_estru_saida-id_item_docnum_s  =      wa_saida-itmnum  .
      wa_estru_saida-data_docnum_s     =      wa_saida-docdat  .
      wa_estru_saida-cfop_docnum_s     =      wa_saida-cfop    .
      wa_estru_saida-docnum_e          =      wa_saida-vbeln_p .
      wa_estru_saida-id_item_docnum_e  =      wa_saida-itmnum_j.
      wa_estru_saida-data_docnum_e     =      wa_saida-pstdat  .
      wa_estru_saida-usuario           =      sy-uname.
      wa_estru_saida-datatual          =      sy-datlo.
      wa_estru_saida-horatual          =      sy-uzeit.


      UPDATE zlft_exp_conv_20 FROM wa_estru_saida.
      COMMIT WORK.
      CLEAR: wa_saida, wa_estru_saida.
    ENDIF.



  ENDLOOP.
ENDFORM.                    " ARMAZENA_DADOS


* Converte data no formato YYYYMMDD, para data no formato DD/MMM/YYYY
* Converts data from format YYYYMMDD to format DD/MMM/YYYY
* -------------------------------------------------------------------
FORM formata_data USING    input_date
                 CHANGING formated_date.
  DATA: int_month     TYPE i,
        day(2),
        month(2),
        year(4),
        month_name(3),
        meses(36).

  meses = 'JanFebMarAprMayJunJulAugSepOctNovDec'.
  day   = input_date+6(2).
  month = input_date+4(2).
  year  = input_date(4).

  IF month = '00'.
    CLEAR formated_date.
  ELSE.
    int_month  = ( month - 1 ) * 3.
    month_name = meses+int_month(3).

    CONCATENATE day '/'
                month_name '/'
                year
           INTO formated_date.
  ENDIF.
ENDFORM.                    "formata_data
