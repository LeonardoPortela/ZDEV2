*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 07/02/2022                                              &*
*& Descrição: JOB para verificar grupos de mercadoria T023            &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zmmr0036.

TYPES: BEGIN OF ty_saida,
         matkl   TYPE t023-matkl,
         wgbez   TYPE t023t-wgbez,
         wgbez60 TYPE t023t-wgbez60.
TYPES: END OF ty_saida.

DATA: git_t023_aux TYPE TABLE OF t023,
      git_t023     TYPE TABLE OF t023,
      git_mara     TYPE TABLE OF mara,
      git_zmmt0039 TYPE TABLE OF zmmt0039,
      git_t023t    TYPE TABLE OF t023t,
      git_saida    TYPE TABLE OF ty_saida,
      gwa_saida    LIKE LINE  OF git_saida.


START-OF-SELECTION.
  SELECT SINGLE COUNT(*) INTO @DATA(vg_job)
      FROM tbtco
     WHERE jobname EQ 'GRUPO_MERCADORIA_SEM_CONTA'
       AND status EQ 'R'.

  IF ( vg_job EQ 1 ).
    PERFORM fm_start_of_selection.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  DATA: lva_data TYPE sy-datum.

  lva_data = sy-datum - 10.

  SELECT *
  FROM mara
  INTO TABLE git_mara
  WHERE ersda >= lva_data
      OR  laeda >= lva_data .

  DELETE git_mara  WHERE  matkl+0(1) NA sy-abcde.

  SELECT *
    FROM t023
    INTO TABLE git_t023_aux
    FOR ALL ENTRIES IN git_mara
    WHERE matkl EQ git_mara-matkl.

  DELETE git_t023_aux  WHERE  matkl+0(1) NA sy-abcde.

  IF  git_t023_aux  IS NOT INITIAL.
    SELECT *
      FROM zmmt0039
      INTO TABLE git_zmmt0039
      FOR ALL ENTRIES IN git_t023_aux
     WHERE matkl EQ git_t023_aux-matkl.

    LOOP AT  git_t023_aux  INTO DATA(lwa_t023_aux).
      READ TABLE  git_zmmt0039 INTO DATA(lwa_zmmt0039) WITH KEY matkl = lwa_t023_aux-matkl.
      IF sy-subrc = 0.
        CONTINUE.
      ELSE.
        APPEND lwa_t023_aux TO  git_t023.
        CLEAR: lwa_t023_aux.
      ENDIF.
    ENDLOOP.
    IF git_t023 IS NOT INITIAL.
      SELECT *
        FROM t023t
        INTO TABLE git_t023t
        FOR ALL ENTRIES IN git_t023
       WHERE matkl EQ git_t023-matkl
        AND  spras EQ sy-langu.


      LOOP AT git_t023 INTO DATA(lwa_t023).
        READ TABLE git_t023t INTO DATA(lwa_t023t) WITH KEY  matkl =  lwa_t023-matkl.

        gwa_saida-matkl   = lwa_t023-matkl.
        gwa_saida-wgbez   = lwa_t023t-wgbez.
        gwa_saida-wgbez60 = lwa_t023t-wgbez60.

        APPEND gwa_saida TO git_saida.

        CLEAR:  lwa_t023, lwa_t023t, gwa_saida.

      ENDLOOP.

      PERFORM fm_dados_processa.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

*-----------------------------------------------
*-Destinatarios
*-----------------------------------------------
  DATA:
    lit_tvarvc       TYPE TABLE OF tvarvc,
    lwa_tvarvc       TYPE tvarvc,
    lwa_destinatario TYPE somlrec90,
    lit_destinatario TYPE TABLE OF somlrec90.

  DATA: lva_titulo  TYPE string,
        lva_matkl   TYPE string,
        lva_wgbez   TYPE string,
        lva_wgbez60 TYPE string,
        lva_desc    TYPE string.

  DATA: doc_chng    LIKE sodocchgi1.
  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: lit_html    TYPE TABLE OF w3html INITIAL SIZE 0 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.

  SELECT  *
    FROM tvarvc
    INTO TABLE lit_tvarvc
   WHERE name = 'Z_GRUPO_MATERIAL_MAIL'.

  REFRESH: reclist.
  LOOP AT lit_tvarvc        INTO lwa_tvarvc.
    CONDENSE  lwa_tvarvc-low.
    TRANSLATE lwa_tvarvc-low  TO LOWER CASE.

    reclist-receiver = lwa_tvarvc-low.
    reclist-rec_type = 'U'.
    APPEND reclist.
  ENDLOOP.

  DEFINE conc_html.
    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        I_TEXTO = &1
      TABLES
        IT_HTML = LIT_HTML.
  END-OF-DEFINITION.


  CLEAR: lva_titulo.
  lva_titulo = 'Atenção! Grupo de Mercadoria sem classificação contábil na transação ZMM0048!'.

  "Monta Corpo Email
  conc_html '<html>'.
  conc_html '<head><title>Grupo de Mercadoria sem classificação contábil na transação ZMM0048</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  conc_html '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
  conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
  conc_html lva_titulo.
  conc_html '</STRONG></FONT></DIV><BR>'.
  conc_html '<FONT face=Verdana color=#0000ff size=2>'.
  conc_html '<BR>'.

  conc_html '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="50%">'.

  conc_html    '<tr bgcolor="B0C4DE">'.
  conc_html       '<td width="10%"><p align="center"><font color="#000030" size=1><b>Grupos de Mercadoria</b></font></p></td>'.
  conc_html       '<td width="40%"><p align="center"><font color="#000030" size=1><b>Descrição  </b></font></p></td>'.
  conc_html    '</tr>'.

  LOOP AT git_saida INTO gwa_saida.


    lva_matkl    = gwa_saida-matkl.
    lva_wgbez    = gwa_saida-wgbez.
    lva_wgbez60  = gwa_saida-wgbez60.
    CONCATENATE lva_wgbez '-' lva_wgbez60 INTO lva_desc.

    conc_html  '<tr bordercolor="black">'.

    conc_html     '<td width="10%"><p align="left"> <font color="000" size=1><b>'.
    conc_html        lva_matkl.
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="50%"><p align="left"> <font color="000" size=1><b>'.
    conc_html       lva_desc.
    conc_html     '</b></font></p></td>'.

  ENDLOOP.

  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num = 0.
  objpack-body_start = 1.
  objpack-body_num = 99999.
  objpack-doc_type = 'HTM'.
  APPEND objpack.

  "Corpo
  doc_chng-obj_name = 'Grupo de Mercadoria sem classificação contábil na transação ZMM0048'.
  doc_chng-obj_descr = 'Aviso Grupo Mercadoria sem class. cont. ZMM0048'.
  doc_chng-no_change = 'X'.


*-----------------------------------------------
*-Envio email
*-----------------------------------------------
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      contents_txt               = lit_html
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.



ENDFORM.
