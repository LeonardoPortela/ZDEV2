
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 26/09/2018                                              &*
*& Descrição: Relatório de Pendências de Retorno                      &*
*& Transação:                                                         &*
*---------------------------------------------------------------------&*

REPORT  zmmr145.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: j_1bnfdoc.

TYPES:
  BEGIN OF ty_j_1bnflin,
    bukrs  TYPE j_1bnfdoc-bukrs,
    branch TYPE j_1bnfdoc-branch,
    docnum TYPE j_1bnfdoc-docnum,
    nfenum TYPE j_1bnfdoc-nfenum,
    docdat TYPE j_1bnfdoc-docdat,
    partyp TYPE j_1bnfdoc-partyp,
    parid  TYPE j_1bnfdoc-parid,
    matnr  TYPE j_1bnflin-matnr,
    maktx  TYPE j_1bnflin-maktx,
    menge  TYPE j_1bnflin-menge,
    netpr  TYPE j_1bnflin-netpr,
    netwr  TYPE j_1bnflin-netwr,
    cfop   TYPE j_1bnflin-cfop,
    itmnum TYPE j_1bnflin-itmnum,
  END OF ty_j_1bnflin,

  BEGIN OF ty_saida,
    bukrs    TYPE j_1bnfdoc-bukrs,
    branch   TYPE j_1bnfdoc-branch,
    seq_lcto TYPE zfiwrt0008-seq_lcto,
    docnum   TYPE j_1bnfdoc-docnum,
    nfenum   TYPE j_1bnfdoc-nfenum,
    docdat   TYPE j_1bnfdoc-docdat,
    parid    TYPE j_1bnfdoc-parid,
    name1    TYPE lfa1-name1,
    matnr    TYPE j_1bnflin-matnr,
    maktx    TYPE j_1bnflin-maktx,
    menge    TYPE j_1bnflin-menge,
    netpr    TYPE j_1bnflin-netpr,
    netwr    TYPE j_1bnflin-netwr,
    anln1    TYPE zfiwrt0009-anln1,
    anln2    TYPE zfiwrt0009-anln2,
    dias     TYPE i,
    docref   TYPE zfiwrt0008-docref,
  END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: t_bdc          TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab      TYPE TABLE OF bdcmsgcoll,

      it_zfiwrt0008  TYPE TABLE OF zfiwrt0008,
      it_zfiwrt0008r TYPE TABLE OF zfiwrt0008,
      it_zfiwrt0009  TYPE TABLE OF zfiwrt0009,
      it_j_1bnflin   TYPE TABLE OF ty_j_1bnflin,
      it_saida       TYPE TABLE OF ty_saida,
      it_color       TYPE TABLE OF lvc_s_scol.

 data:  W_del type ZFIWRT0001DEL,
        w_docnum type j_1bnfdoc-docnum.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: ok-code       TYPE sy-ucomm,

      wa_t001       TYPE t001,
      wa_j_1bnflin  TYPE ty_j_1bnflin,
      wa_zfiwrt0008 TYPE zfiwrt0008,
      wa_zfiwrt0009 TYPE zfiwrt0009,
      wa_saida      TYPE ty_saida,
      wa_color      TYPE lvc_s_scol,
      w_zfiwrt0008  TYPE zfiwrt0008,
      v_zfiwrt0008  TYPE zfiwrt0008,
      w_zfiwrt0001  TYPE zfiwrt0001.
*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat   TYPE TABLE OF ty_estrutura,
  t_top     TYPE slis_t_listheader,
  xs_events TYPE slis_alv_event,
  events    TYPE slis_t_event,
  gd_layout TYPE slis_layout_alv,
  t_print   TYPE slis_print_alv,
  v_report  LIKE sy-repid,
  t_sort    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  estrutura TYPE TABLE OF ty_estrutura,
  v_repid   LIKE sy-repid.


DATA: list_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------*
* Fim Estrutura dos dados Dinamicos
*----------------------------------------------------------------------*

CONSTANTS c_x               TYPE c VALUE 'X'.

DEFINE mc_preenche_class.
  VG_I = VG_I + 1.
  CLEAR T_SORT.
  T_SORT-SPOS      = VG_I.
  T_SORT-FIELDNAME = &1.
  T_SORT-GROUP     = &2.
  T_SORT-UP        = &3.
  T_SORT-SUBTOT    = &4.
  APPEND T_SORT.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:     p_bukrs    TYPE j_1bnfdoc-bukrs OBLIGATORY.
SELECT-OPTIONS: s_branch   FOR j_1bnfdoc-branch,
                s_pstdat   FOR j_1bnfdoc-pstdat OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK b1.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            f_iniciar_variaves, " Cabeçalho
            f_seleciona_dados, " Form seleciona dados
            f_saida, " Form de saida
            f_imprime_dados.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  SELECT  j_1bnfdoc~bukrs,j_1bnfdoc~branch,j_1bnfdoc~docnum,j_1bnfdoc~nfenum,j_1bnfdoc~docdat,
          j_1bnfdoc~partyp, j_1bnfdoc~parid,
          j_1bnflin~matnr,j_1bnflin~maktx,j_1bnflin~menge,j_1bnflin~netpr,j_1bnflin~netwr, j_1bnflin~cfop, j_1bnflin~itmnum
    FROM j_1bnflin
    INNER JOIN j_1bnfdoc
    ON j_1bnfdoc~docnum = j_1bnflin~docnum
    INTO TABLE @it_j_1bnflin
    WHERE j_1bnfdoc~bukrs  EQ @p_bukrs
    AND   j_1bnfdoc~branch IN @s_branch
    AND   j_1bnfdoc~pstdat IN @s_pstdat
    AND   j_1bnflin~cfop   IN ( '5915AA', '6915AA', '5915aa', '6915aa' )
*----CS1077047 / IR132056 ---->
    and   j_1bnfdoc~CANCEL ne 'X'.

*--- Inclusão de novos docnums deve ser feito mediante IR----
* Não foi usado a clausula NOT IN para não comprometer a performance
* do select da J_1BNFDOC.
  SELECT * FROM ZFIWRT0001DEL into W_del WHERE BUKRS EQ p_bukrs.
      delete it_j_1bnflin where docnum eq w_del-docnum.
  endselect.
*<----CS1077047 / IR132056 ----


  IF it_j_1bnflin[] IS NOT INITIAL.

    SELECT *
      FROM zfiwrt0008
      INTO TABLE it_zfiwrt0008r "Retorno
      FOR ALL ENTRIES IN it_j_1bnflin
      WHERE docref EQ it_j_1bnflin-docnum.


    SELECT *
      FROM zfiwrt0008
      INTO TABLE it_zfiwrt0008
      FOR ALL ENTRIES IN it_j_1bnflin
      WHERE docnum EQ it_j_1bnflin-docnum.



    IF it_zfiwrt0008[] IS NOT INITIAL.
      SELECT *
        FROM zfiwrt0009
        INTO TABLE it_zfiwrt0009
        FOR ALL ENTRIES IN it_zfiwrt0008
        WHERE seq_lcto EQ it_zfiwrt0008-seq_lcto.

    ENDIF.
  ENDIF.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .
  SORT: it_zfiwrt0008 BY docnum,
        it_zfiwrt0008r BY docref,
        it_zfiwrt0009 BY seq_lcto matnr.

  LOOP AT it_j_1bnflin INTO wa_j_1bnflin.
    READ TABLE it_zfiwrt0008r INTO wa_zfiwrt0008 WITH KEY docref = wa_j_1bnflin-docnum BINARY SEARCH.
    IF sy-subrc NE 0.
*---> CS1065327 / IR126917 --->
      READ TABLE it_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY docref = wa_j_1bnflin-docnum BINARY SEARCH.

      IF sy-subrc EQ 0.
        CONTINUE.
      ELSE.

        SELECT SINGLE * FROM zfiwrt0008 INTO w_zfiwrt0008
             WHERE docnum EQ wa_j_1bnflin-docnum.

        IF sy-subrc EQ 0.

          SELECT SINGLE * FROM zfiwrt0001 INTO w_zfiwrt0001
                 WHERE operacao EQ w_zfiwrt0008-operacao.

          IF w_zfiwrt0001-retorno EQ 'N'.

            SELECT SINGLE * FROM zfiwrt0008 INTO w_zfiwrt0008
              WHERE docref EQ w_zfiwrt0008-seq_lcto
               AND bukrs EQ w_zfiwrt0008-bukrs
               AND branch EQ w_zfiwrt0008-branch
               AND parid EQ w_zfiwrt0008-parid.

            IF sy-subrc EQ 0.
              SELECT SINGLE * FROM zfiwrt0001 INTO w_zfiwrt0001
                 WHERE operacao EQ w_zfiwrt0008-operacao.

              IF w_zfiwrt0001-retorno EQ 'S'.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
*<--- CS1065327 / IR126917 <---
      ENDIF.
    ELSE.
      CONTINUE. "ja retornou, entao não exibe
    ENDIF.
    MOVE-CORRESPONDING wa_j_1bnflin TO wa_saida.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_saida-matnr
      IMPORTING
        output = wa_saida-matnr.

    IF wa_j_1bnflin-partyp = 'V'.
      SELECT SINGLE name1 INTO wa_saida-name1
        FROM lfa1
        WHERE lifnr = wa_saida-parid.
    ELSEIF wa_j_1bnflin-partyp = 'C'.
      SELECT SINGLE name1
         INTO wa_saida-name1
         FROM kna1
         WHERE kunnr = wa_saida-parid.
    ELSE.
      SELECT SINGLE name1
        INTO wa_saida-name1
        FROM t001w
        WHERE werks = wa_saida-parid+6(4).
    ENDIF.
    READ TABLE it_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-seq_lcto = wa_zfiwrt0008-seq_lcto.
      wa_saida-docref  =  wa_zfiwrt0008-docref.
    ENDIF.
    READ TABLE it_zfiwrt0009 INTO wa_zfiwrt0009 WITH KEY seq_lcto = wa_saida-seq_lcto
                                                         matnr    = wa_j_1bnflin-matnr
                                                         itmnum   = wa_j_1bnflin-itmnum  BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-anln1 = wa_zfiwrt0009-anln1.
      wa_saida-anln2 = wa_zfiwrt0009-anln2.
    ENDIF.
    wa_saida-dias = sy-datum  - wa_saida-docdat.
    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.

  ENDLOOP.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .
  PERFORM f_definir_eventos.
  PERFORM f_alv.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                     slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM top_of_page.

* Cabeçalho Logo
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = list_top_of_page[].
  "I_LOGO             = 'WELLA_LOGO'.

ENDFORM.        " top_of_page.

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort .

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .

  FREE: it_fcat.
  PERFORM alv_preenche_cat USING:
               'BUKRS'            'Empresa'           '10'  ' '  ' '  ' ' ,
               'BRANCH'           'Filial'            '10'  ' '  ' '  ' ' ,
               'SEQ_LCTO'         'Doc.ZNFW'          '10'  ' '  ' '  ' ' ,
               'DOCNUM'           'DocNum'            '10'  ' '  ' '  ' ' ,
               'NFENUM'           'N° NF-e'           '10'  ' '  ' '  ' ' ,
               'DOCREF'           'Doc.Original'      '10'  ' '  ' '  ' ' ,
               'DOCDAT'           'Data Emissão'      '10'  ' '  ' '  ' ' ,
               'PARID'            'Cód.Forn.'         '10'  ' '  ' '  ' ' ,
               'NAME1'            'Nome Fornecedor'   '30'  ' '  ' '  ' ' ,
               'MATNR'            'Cód. Produto'      '10'  ' '  ' '  ' ' ,
               'ANLN1'            'Imobilizado'       '10'  ' '  ' '  ' ' ,
               'ANLN2'            'Sub num'           '07'  ' '  ' '  ' ' ,
               'MAKTX'            'Descrição Produto' '30'  ' '  ' '  ' ' ,
               'MENGE'            'Quantidade'        '12'  ' '  ' '  ' ' ,
               'NETPR'            'Valor Unit'        '15'  ' '  ' '  ' ' ,
               'NETWR'            'Valor Total'       '15'  ' '  ' '  ' ' ,
               'DIAS'             'Qtd de dias'       '10'  ' '  ' '  ' ' .

  PERFORM f_imprime_dados_alv.



ENDFORM.

FORM f_imprime_dados_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      is_layout                = gd_layout
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = it_fcat[]
      it_sort                  = t_sort[]
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
    TABLES
      t_outtab                 = it_saida.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.        "#EC CALLED
  DESCRIBE TABLE rt_extab. "Avoid Extended Check Warning
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "Set_pf_status

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*

FORM user_command USING r_ucomm     LIKE sy-ucomm           "#EC CALLED
                        rs_selfield TYPE slis_selfield.

  DATA: vl_nfobjn TYPE j_1binterf-nfobjn,
        vl_docnum TYPE j_1bnfdoc-docnum.

  IF r_ucomm = '&F03' OR r_ucomm = '&F15' OR r_ucomm = '&F12'.
    EXIT.
  ELSE.
    CASE rs_selfield-fieldname.
      WHEN 'SEQ_LCTO'.
        READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.

        IF wa_saida-seq_lcto IS NOT INITIAL.
          SET PARAMETER ID 'SEQ' FIELD  wa_saida-seq_lcto.
          CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DOCNUM'.
        vl_docnum = rs_selfield-value.

        CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
          EXPORTING
            doc_number         = vl_docnum
          IMPORTING
            obj_number         = vl_nfobjn
          EXCEPTIONS
            document_not_found = 1
            docum_lock         = 2
            OTHERS             = 3.

        CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
          EXPORTING
            obj_number         = vl_nfobjn
          EXCEPTIONS
            object_not_found   = 1
            scr_ctrl_not_found = 2
            OTHERS             = 3.
    ENDCASE.
  ENDIF.



ENDFORM.                    "USER_COMMAND
"USER_COMMAND

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM f_monta_top_of_page USING p_list_top_of_page TYPE
                               slis_t_listheader.

  DATA: t_header   TYPE slis_listheader,
        v_data(10) TYPE c.

  t_header-typ  = 'H'.
  t_header-info = 'Relatório de pendencias de retorno de conserto' .
  APPEND t_header TO p_list_top_of_page.
  CLEAR t_header.
  WRITE sy-datum USING EDIT  MASK '__.__.____' TO v_data.
  CONCATENATE 'Data : '(023)  v_data INTO t_header-key SEPARATED BY
  space.
  t_header-typ  = 'S'.
  APPEND t_header TO p_list_top_of_page.

ENDFORM.                    " f_monta_top_of_page

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo  TYPE c
                               p_desc   TYPE c
                               p_tam    TYPE c
                               p_hot    TYPE c
                               p_zero   TYPE c
                               p_soma   TYPE c.

  DATA: wl_fcat TYPE ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.
  IF ( ( p_campo EQ 'DOCNUM' ) OR  ( p_campo EQ 'SEQ_LCTO' )  ) .
    wl_fcat-hotspot = 'X'.
  ELSE.
    CLEAR wl_fcat-hotspot.
  ENDIF.
  APPEND wl_fcat TO it_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves .
  DATA:
    w_texto1(10),
    w_texto2(10),
    w_texto3(40),

    w_empresa_texto(40),


    empresa             TYPE c LENGTH 99,
    cabec               TYPE c LENGTH 50.


  v_report = sy-repid.
  w_texto3 = 'Pendências de retorno'.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.
  SELECT SINGLE *
         FROM t001
         INTO wa_t001
  WHERE bukrs = p_bukrs.
  IF p_bukrs IS NOT INITIAL.
    CONCATENATE  p_bukrs '-' wa_t001-butxt INTO empresa SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' empresa.
  ENDIF.

  CLEAR w_texto3.
  IF s_branch-low IS NOT INITIAL.
    CONCATENATE  'Filial:' s_branch-low INTO w_texto3 SEPARATED BY space.
  ENDIF.
  IF s_branch-high IS NOT INITIAL.
    CONCATENATE  w_texto3 'à' s_branch-high INTO w_texto3 SEPARATED BY space.
  ENDIF.
  IF w_texto3 IS NOT INITIAL.
    PERFORM f_construir_cabecalho USING 'S' w_texto3.
  ENDIF.


  CONCATENATE  s_pstdat-low+6(2) '.'  s_pstdat-low+4(2) '.' s_pstdat-low+0(4) INTO w_texto1.
  CONCATENATE  'Data:' w_texto1 INTO w_texto3 SEPARATED BY space.
  IF s_pstdat-high IS NOT INITIAL.
    CONCATENATE  s_pstdat-high+6(2) '.'  s_pstdat-high+4(2) '.' s_pstdat-high+0(4) INTO w_texto1.
    CONCATENATE  w_texto3 'à' w_texto1 INTO w_texto3 SEPARATED BY space.
  ENDIF.
  PERFORM f_construir_cabecalho USING 'S' w_texto3.


ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  f_construir_cabecalho
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TYP        text
*      -->TEXT       text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho    USING typ text.


  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: w_answer(1),
        wa_zglt090  TYPE zglt090,
        it_zglt090  TYPE TABLE OF zglt090.

  CASE ok-code.
    WHEN 'SAIR' OR 'EXIT'.
      SET SCREEN 0.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
