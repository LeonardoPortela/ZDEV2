*----------------------------------------------------------------------*
*                   AMAGGI                                             *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZLESR0157                                               *
* Descrição  : Job envio de e-mail                                     *
* Módulo     : LES                                        Transação:   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Fernando W. Luvizotte                  Data: 13/06/2012 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
REPORT zlesr0157_bkp.


TABLES: zlest0039,zlest0225.

TYPES:
  BEGIN OF ty_saida,
    cd_fil       TYPE werks, "Filial Cód
    nm_fil       TYPE name1, "Filial Nome
    cd_prod      TYPE matnr, "Produto Cód
    cd_nfe       TYPE j_1bnfdoc-nfenum, "NFE
    cd_placa     TYPE zlest0039-placa_cav, "Placa
    dt_saida(12) TYPE c, "Data Saida
    kg_saida     TYPE string, "Peso Saida
    dias_dif     TYPE string, "DIAS TRANSITO
    nm_trans     TYPE kna1-name1, "Transbordo
    nm_port      TYPE lfa1-name1, "Porto
    nm_obs       TYPE zlest0039-observacao, "Observação
  END OF ty_saida.

TYPES: BEGIN OF ty_225,
         bukrs TYPE bukrs,
         email TYPE zemail255,
       END OF ty_225,

       BEGIN OF ty_225_werks,
         bukrs TYPE bukrs,
         werks TYPE werks_d,
       END OF ty_225_werks,

       BEGIN OF ty_lfa1,
         lifnr TYPE lifnr,
         name1 TYPE name1,
       END OF ty_lfa1.


DATA: tg_225              TYPE TABLE OF ty_225,
      tg_225_werks        TYPE TABLE OF ty_225_werks,
      tg_j1bnflin         TYPE TABLE OF j_1bnflin,
      tg_j1bnfdoc         TYPE TABLE OF j_1bnfdoc,
      tg_zsdt_export      TYPE TABLE OF zsdt_export,
      tg_zsdt_retlote     TYPE TABLE OF zsdt_retlote,
      tg_tvarvc           TYPE TABLE OF tvarvc,
      tg_zlest0039        TYPE TABLE OF zlest0039,
      tg_zlest0226        TYPE TABLE OF zlest0226,
      tg_zlest0225        TYPE TABLE OF zlest0225,
      tg_tg_zlest0225_aux TYPE TABLE OF zlest0225,
      tg_t001w            TYPE TABLE OF t001w,
      tg_kna1             TYPE TABLE OF kna1,
      tg_lfa1             TYPE TABLE OF ty_lfa1,
      wg_zlest0039        LIKE LINE OF tg_zlest0039,
      wg_j1bnflin         LIKE LINE OF tg_j1bnflin,
      wg_j1bnfdoc         LIKE LINE OF tg_j1bnfdoc,
      wg_tvarvc           LIKE LINE OF tg_tvarvc,
      wg_zlest0226        LIKE LINE OF tg_zlest0226,
      wg_zlest0225        LIKE LINE OF tg_zlest0225,
      wg_kna1             LIKE LINE OF tg_kna1,
      wg_225              LIKE LINE OF tg_225,
      wg_225_werks        LIKE LINE OF tg_225_werks,
      wg_lfa1             LIKE LINE OF tg_lfa1,
      wg_t001w            LIKE LINE OF tg_t001w.

DATA: r_exe        TYPE RANGE OF zexdiario,
      v_observacao TYPE string,
      it_saida     TYPE STANDARD TABLE OF ty_saida WITH HEADER LINE,
      it_saida_aux TYPE STANDARD TABLE OF ty_saida WITH HEADER LINE,
      aux_werks    TYPE STANDARD TABLE OF werks WITH HEADER LINE,
      it_filiais   TYPE STANDARD TABLE OF werks WITH HEADER LINE.


DATA dt_exec(12) TYPE c.
DATA dt_exec_sendfile(10) TYPE c.
DATA nm_sendfile TYPE string.
DATA nm_titulo TYPE so_obj_des.
DATA nm_subject TYPE sood-objdes.
DATA lv_size     TYPE i.
DATA     lr_filial TYPE RANGE OF werks.

FIELD-SYMBOLS: <it_saida>.
DATA email TYPE string.

DATA: lt_mailsubject     TYPE sodocchgi1,
      lt_mailrecipientes TYPE STANDARD TABLE OF somlrec90 WITH HEADER LINE,
      lt_mailtxt         TYPE STANDARD TABLE OF soli INITIAL SIZE 0.


DATA: lt_mail         TYPE STANDARD TABLE OF tvarvc,
      email_addresses TYPE adr6-smtp_addr.

DATA: total       TYPE i,
      peso        TYPE i,
      v_peso(15)  TYPE c,
      v_total(15) TYPE c.

DATA: rg_cfop TYPE RANGE OF j_1bcfop.


DATA main_text      TYPE bcsy_text.

DATA : it_contents TYPE STANDARD TABLE OF solisti1,
       html        TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE.
DATA lt_att_head TYPE soli_tab.
DATA xhtml_string TYPE xstring.
DATA t_hex TYPE solix_tab.


DATA :
  document     TYPE REF TO cl_document_bcs,
  send_request TYPE REF TO cl_bcs,
  recipient    TYPE REF TO if_recipient_bcs,
  lo_ex_bcs    TYPE REF TO cx_bcs.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002  .
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_dtsai TYPE zlest0039-datasaida.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003  .
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1 .
    PARAMETERS: p_exe AS CHECKBOX DEFAULT '' USER-COMMAND abc.
    SELECTION-SCREEN COMMENT 05(20) TEXT-005 FOR FIELD p_exe.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  PERFORM zf_limpa_dados.

  PERFORM zf_seleciona_tvarv.

  PERFORM zf_seleciona_dados.

  PERFORM zf_prepara_dados.

  PERFORM envia.

  "PERFORM zf_envia_emails.

  "PERFORM zf_envia_emails_werks.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM zf_seleciona_dados .
  DATA: p_dt_saida    TYPE  sy-datum,
        lv_datalimite TYPE sy-datum.

  DATA: r_date   TYPE RANGE OF sy-datum,
        r_status TYPE RANGE OF zstatus_comp.

  DATA: r_docnum TYPE RANGE OF zlest0039-docnum.
  SELECT SINGLE * INTO wg_zlest0226
  FROM zlest0226.

  CHECK wg_zlest0226 IS NOT INITIAL.

  lv_datalimite = sy-datum - wg_zlest0226-dias.
  r_date = VALUE #( ( sign = 'I' option = 'BT' low = p_dtsai high = lv_datalimite ) ).

  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ET' ) TO r_status.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'L1' ) TO r_status.


  SELECT * INTO TABLE tg_zlest0039
    FROM zlest0039
    WHERE  status     IN r_status
     AND datatransb  = '00000000' "Ajuste na seleção de dados / aoenning / 05-09-2023
     AND ck_estornar_trans <> 'X'
     AND datachegada = '00000000' "Ajuste na seleção de dados / aoenning / 05-09-2023
     AND datasaida >= p_dtsai
     AND datasaida <= lv_datalimite.

  IF sy-subrc EQ 0.
    SORT tg_zlest0039 BY bukrs ASCENDING.

    SELECT * INTO TABLE tg_j1bnflin
      FROM j_1bnflin AS a
      FOR ALL ENTRIES IN tg_zlest0039
      WHERE docnum = tg_zlest0039-docnum
        AND cfop IN rg_cfop.

    IF sy-subrc NE 0.
      STOP.
      MESSAGE TEXT-001 TYPE 'E' DISPLAY LIKE 'E'.
    ENDIF.

    SELECT * INTO TABLE tg_j1bnfdoc
      FROM j_1bnfdoc
      FOR ALL ENTRIES IN tg_j1bnflin
      WHERE docnum = tg_j1bnflin-docnum.

    DELETE ADJACENT DUPLICATES FROM tg_j1bnfdoc COMPARING docnum.



    IF tg_j1bnfdoc IS NOT INITIAL.
      SELECT * FROM zsdt_retlote
        INTO TABLE tg_zsdt_retlote
        FOR ALL ENTRIES IN tg_j1bnfdoc
       WHERE docnum EQ tg_j1bnfdoc-docnum.
    ENDIF.

    IF tg_zsdt_retlote IS NOT INITIAL.
      SELECT * FROM zsdt_export
        INTO TABLE tg_zsdt_export
        FOR ALL ENTRIES IN tg_zsdt_retlote
       WHERE docnum EQ tg_zsdt_retlote-docnum_ret.
    ENDIF.

    r_docnum = VALUE #( FOR l IN tg_j1bnflin ( sign = 'I' option = 'EQ' low = l-docnum ) ).
    IF r_docnum IS NOT INITIAL.
      DELETE tg_zlest0039 WHERE docnum NOT IN r_docnum.
    ENDIF.
  ELSE.
    STOP.
    MESSAGE TEXT-001 TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.

  SORT tg_zlest0039 BY werks ASCENDING.

*  IF p_exe IS NOT INITIAL.

  SELECT * INTO TABLE tg_zlest0225
    FROM zlest0225
    FOR ALL ENTRIES IN tg_zlest0039
    WHERE bukrs = tg_zlest0039-bukrs
      AND exediario EQ p_exe.
*  ELSE.
*
*    SELECT * INTO TABLE tg_zlest0225
*      FROM zlest0225
*      FOR ALL ENTRIES IN tg_zlest0039
*      WHERE bukrs = tg_zlest0039-bukrs
*        AND exediario EQ space.
*
*  ENDIF.



  SORT tg_zlest0225 BY bukrs ASCENDING.

  "Sem centro
  LOOP AT  tg_zlest0225 INTO wg_zlest0225 WHERE werks IS INITIAL.
    MOVE-CORRESPONDING wg_zlest0225 TO wg_225.
    APPEND wg_225 TO tg_225.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM tg_225 COMPARING bukrs email.

  "Com o centro
  LOOP AT  tg_zlest0225 INTO wg_zlest0225 WHERE werks <> ''.
    MOVE-CORRESPONDING wg_zlest0225 TO wg_225_werks.
    APPEND wg_225_werks TO tg_225_werks.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM tg_225_werks COMPARING bukrs werks.

  SELECT * INTO TABLE tg_kna1
  FROM kna1
  FOR ALL ENTRIES IN tg_zlest0039
  WHERE kunnr EQ tg_zlest0039-pontotransb.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE tg_lfa1
    FROM lfa1
    FOR ALL ENTRIES IN tg_zlest0039
    WHERE lifnr  = tg_zlest0039-pontoentrega.


  "Descrição da filial.
  SELECT * FROM t001w INTO TABLE tg_t001w FOR ALL ENTRIES IN tg_zlest0039 WHERE spras EQ sy-langu AND werks EQ tg_zlest0039-werks.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPA_DADOS
*&---------------------------------------------------------------------*
FORM zf_limpa_dados .

  CLEAR: tg_j1bnflin,
         tg_j1bnfdoc,
         tg_tvarvc,
         tg_zlest0039,
         tg_zlest0226,
         tg_zlest0225,
         tg_kna1,
         tg_225,
         tg_225_werks,
         tg_lfa1[],
         rg_cfop[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_TVARV
*&---------------------------------------------------------------------*
FORM zf_seleciona_tvarv .

  DATA: w_cfop LIKE LINE OF rg_cfop.

  CONSTANTS: c_name TYPE rvari_vnam VALUE 'Z_ZCFOP_ZLES0050'.

  SELECT * INTO TABLE tg_tvarvc
    FROM tvarvc
    WHERE name = c_name.
  IF sy-subrc EQ 0.
    LOOP AT tg_tvarvc INTO wg_tvarvc.
      w_cfop-sign = 'I'.
      w_cfop-option = 'EQ'.
      w_cfop-low = wg_tvarvc-low.

      "retirar a barra.
      REPLACE ALL OCCURRENCES OF '/' IN w_cfop-low WITH ' '.

      APPEND w_cfop TO rg_cfop.
    ENDLOOP.
  ENDIF.

ENDFORM.

*...Prepara Dados

FORM zf_prepara_dados .

  READ TABLE tg_zlest0226 INTO wg_zlest0226 INDEX 1.
  CHECK  tg_zlest0039 IS NOT INITIAL.
  SORT  tg_zlest0039 BY bukrs werks pontoentrega datasaida.

  LOOP AT tg_zlest0039 INTO wg_zlest0039
      WHERE bukrs = wg_225-bukrs.

    READ TABLE tg_j1bnflin INTO wg_j1bnflin
    WITH KEY docnum = wg_zlest0039-docnum.

    READ TABLE tg_j1bnfdoc INTO wg_j1bnfdoc
    WITH KEY docnum = wg_zlest0039-docnum.

    READ TABLE tg_zsdt_retlote INTO DATA(wg_zsdt_retlote)
    WITH KEY docnum = wg_j1bnfdoc-docnum.

    IF sy-subrc EQ 0.
      READ TABLE tg_zsdt_export INTO DATA(wg_zsdt_export)
      WITH KEY docnum = wg_zsdt_retlote-docnum_ret.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR wg_kna1.
    READ TABLE tg_kna1 INTO wg_kna1
    WITH KEY kunnr = wg_zlest0039-pontotransb.

    READ TABLE tg_lfa1 INTO wg_lfa1
    WITH KEY lifnr = wg_zlest0039-pontoentrega.

    READ TABLE tg_t001w INTO wg_t001w
    WITH KEY werks = wg_zlest0039-werks.

    CLEAR: wg_zsdt_retlote, wg_zsdt_export, wg_j1bnfdoc, wg_j1bnflin.


    it_saida-cd_fil = wg_zlest0039-werks."Filial Cód
    it_saida-nm_fil = wg_t001w-name1."Filial Nome
    it_saida-cd_placa = wg_zlest0039-placa_cav. "Placa
    it_saida-dt_saida = |{ wg_zlest0039-datasaida+6(2) }/{ wg_zlest0039-datasaida+4(2) }/{ wg_zlest0039-datasaida+0(4) }|. "Data Saida
    CONDENSE it_saida-dt_saida NO-GAPS.
    it_saida-dias_dif = sy-datum - wg_zlest0039-datasaida. "DIAS TRANSITO
    CONDENSE it_saida-dias_dif NO-GAPS.
    it_saida-nm_trans = wg_kna1-name1. "Transbordo
    it_saida-nm_port = wg_lfa1-name1. "Porto

    PACK wg_zlest0039-nfenum TO it_saida-cd_nfe. "NFE
    PACK wg_zlest0039-matnr TO it_saida-cd_prod. "Produto Cód
    IF wg_zlest0039-pesosaida IS NOT INITIAL.

      it_saida-kg_saida = wg_zlest0039-pesosaida. "Peso Saida.
    ELSE.
      it_saida-kg_saida = 0.

    ENDIF.

    REPLACE '.' IN it_saida-kg_saida WITH ',' .
    REPLACE ',000' IN it_saida-kg_saida WITH '' .

    IF wg_zlest0039-observacao IS NOT INITIAL.
      it_saida-nm_obs = wg_zlest0039-observacao. "Observação
    ELSE.
      it_saida-nm_obs = 'Sem Histórico'.
    ENDIF.


    APPEND it_saida.


  ENDLOOP.

  SORT it_saida BY cd_fil ASCENDING.

ENDFORM.

FORM envia.

**********************************************************************
*Envia E-mail
**********************************************************************

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<filiais>).
    aux_werks = <filiais>-cd_fil.
    APPEND aux_werks.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM aux_werks.

  SORT aux_werks BY werks ASCENDING.

  SORT tg_zlest0225 BY werks.

  DATA(tg_full) = tg_zlest0225.
  DATA(tg_segregado) = tg_zlest0225.

  DELETE tg_full WHERE werks IS NOT INITIAL.
  DELETE ADJACENT DUPLICATES FROM tg_full COMPARING email.
  DELETE tg_segregado WHERE werks IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM tg_segregado COMPARING email.


  IF tg_full IS NOT INITIAL. "Envia Todas Unidades se Werks estiver vazio

    LOOP AT tg_full ASSIGNING FIELD-SYMBOL(<full>) WHERE werks IS INITIAL.

      email_addresses = <full>-email.
      TRANSLATE email_addresses TO LOWER CASE.
      CONDENSE email_addresses NO-GAPS.

      APPEND VALUE #( receiver = email_addresses  rec_type =   'U'  rec_date =   '00000000' ) TO lt_mailrecipientes.

    ENDLOOP.

    it_filiais[] = aux_werks[].

    PERFORM monta_email.

    PERFORM email_todos.

    CLEAR: email,lt_mailrecipientes, email_addresses, v_total, total, it_contents,lv_size,lt_mailrecipientes,lt_att_head,send_request,document, send_request, recipient.
    FREE: lt_mailrecipientes[],it_filiais,it_filiais[], it_contents, html.
  ENDIF.


  IF tg_segregado IS NOT INITIAL. "Envia somente para a unidade da werks

    FREE: tg_tg_zlest0225_aux[].
    tg_tg_zlest0225_aux[] = tg_segregado[].
    SORT tg_tg_zlest0225_aux[] BY email.
    DELETE ADJACENT DUPLICATES FROM tg_tg_zlest0225_aux[] COMPARING email.

    LOOP AT tg_tg_zlest0225_aux INTO DATA(ws_lest0225_aux).
      LOOP AT tg_segregado ASSIGNING FIELD-SYMBOL(<segragado>) WHERE email EQ ws_lest0225_aux-email.
        it_filiais-werks = <segragado>-werks.
        APPEND it_filiais.
      ENDLOOP.

      email = <segragado>-email.
      TRANSLATE email TO LOWER CASE.
      CONDENSE email NO-GAPS.

      email_addresses = email.

      APPEND VALUE #( receiver = email  rec_type =   'U'  rec_date =   '00000000' ) TO lt_mailrecipientes.

      PERFORM monta_email.
      PERFORM email_segregado.
      CLEAR: email,lt_mailrecipientes, ws_lest0225_aux, email_addresses, v_total, total, it_contents,lv_size,lt_mailrecipientes,lt_att_head,send_request,document, send_request, recipient.
      FREE: lt_mailrecipientes[],it_filiais,it_filiais[], it_contents, html.
    ENDLOOP.
  ENDIF.

ENDFORM.

FORM monta_email.

*Texto do Corpo E-mail
  nm_sendfile = |Acompanhamento de transito rodoviário|.
  nm_subject = nm_sendfile.

  APPEND '<html>' TO html.
  APPEND '<style>' TO html.
  APPEND 'table { font-size: 14px; font-family: arial, sans-serif; border: 0px solid #ffffff; border-collapse: collapse; text-align: center;}' TO html.
  APPEND 'td{ font-size: 14px; font-family: arial, sans-serif; border: 1px solid #dddddd; text-align: left;}' TO html.
  APPEND 'th{ font-size: 14px; font-family: arial, sans-serif; border: 1px solid #dddddd; text-align: center;}' TO html.
  APPEND '.bg {background-color: #C8E6C9;}' TO html.
  APPEND '</style>' TO html.
  APPEND '</head>' TO html.
  APPEND '<body>' TO html.
  APPEND '<strong  style="color: #000; font-size: 14px, font-weight: bold, margin-buttom: 11px, font-family: Calibri" >' TO html.
  APPEND 'Senhores(as),<br>Segue acompanhamento do volume em trânsito rodoviário. Favor verificar se há pendências da sua unidade e, se <br>' TO html.
  APPEND 'for RECUSA ou SINISTRO, gentileza abrir SRE para regularização do saldo com urgência.</strong><br></br>' TO html.
  APPEND '<table style="width:0%">' TO html.
  APPEND '<tr bgcolor="#E0ECF8" style="color: #000; font-Size: 14px" align="center">'TO html.
  APPEND '<th><strong>Filial</strong></th>' TO html.
  APPEND '<th><strong>Descrição</strong></th>' TO html.
  APPEND '<th><strong>Produto</strong></th>' TO html.
  APPEND '<th><strong>NFe</strong></th>' TO html.
  APPEND '<th><strong>Placa</strong></th>' TO html.
  APPEND '<th><strong>Data Saída</strong></th>' TO html.
  APPEND '<th><strong>Peso Saída</strong></th>' TO html.
  APPEND '<th><strong>Dias</strong></th>' TO html.
  APPEND '<th><strong>Transbordo</strong></th>' TO html.
  APPEND '<th><strong>Porto</strong></th>' TO html.
  APPEND '<th><strong>Observação</strong></th>' TO html.
  APPEND '</tr>' TO html.

  LOOP AT it_filiais ASSIGNING FIELD-SYMBOL(<filiais>).

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<todos>) WHERE cd_fil = <filiais>-werks.
      APPEND '<tr>' TO html.
      CONCATENATE '<td>' <todos>-cd_fil   '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-nm_fil   '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-cd_prod  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-cd_nfe   '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-cd_placa '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-dt_saida '</td>' INTO html.    APPEND html.
      peso = <todos>-kg_saida.
      WRITE  peso TO v_peso EXPONENT 0.
      CONCATENATE '<td>' v_peso '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-dias_dif '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-nm_trans '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-nm_port  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' <todos>-nm_obs   '</td>' INTO html.    APPEND html.
      APPEND '</tr>' TO html.

    ENDLOOP.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<todos_total>) WHERE cd_fil = <filiais>-werks.
      total = <todos_total>-kg_saida + total .
    ENDLOOP.
**********************************************************************
*. Total Agrpador por Empresa
    APPEND '<tr>' TO html.
    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
    WRITE  total TO v_total EXPONENT 0.
    CONCATENATE '<td>' v_total '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
    APPEND '</tr>' TO html.
**********************************************************************
  ENDLOOP.

  CLEAR: peso, total, v_total,v_peso.

  APPEND '</table>' TO html.
  APPEND '</body>' TO html.
  APPEND '</html>' TO html.

  APPEND LINES OF html TO it_contents.

ENDFORM.

FORM email_todos.
  TRY.
      DATA(send_request) = cl_bcs=>create_persistent( ).
      document = cl_document_bcs=>create_document(
      i_type = 'HTM'
      i_text    =  it_contents
      i_subject = |{ nm_sendfile }| ). "Título do e-mail

      send_request->set_document( document ).

      DELETE ADJACENT DUPLICATES FROM lt_mailrecipientes COMPARING receiver.

      LOOP AT lt_mailrecipientes ASSIGNING FIELD-SYMBOL(<address>).
        email_addresses = <address>-receiver.


        recipient = cl_cam_address_bcs=>create_internet_address( email_addresses ).
        send_request->add_recipient( recipient ).


      ENDLOOP.


      DATA(sent_to_all) = send_request->send( i_with_error_screen = 'X' ).

    CATCH cx_root.

  ENDTRY.

  COMMIT WORK.

  CLEAR: sent_to_all, send_request.


ENDFORM.


FORM email_segregado.
  TRY.
      DATA(send_request) = cl_bcs=>create_persistent( ).
      document = cl_document_bcs=>create_document(
      i_type = 'HTM'
      i_text    =  it_contents
      i_subject = |{ nm_sendfile }| ). "Título do e-mail

      send_request->set_document( document ).


      recipient = cl_cam_address_bcs=>create_internet_address( email_addresses ).
      send_request->add_recipient( recipient ).

      DATA(sent_to_all) = send_request->send( i_with_error_screen = 'X' ).

    CATCH cx_root.

  ENDTRY.

  CLEAR: sent_to_all, send_request.
  COMMIT WORK.
ENDFORM.



***********************************************************************HTML INICIO
*
**&---------------------------------------------------------------------*
**&      Form  ZF_ENVIA_EMAILS
**&---------------------------------------------------------------------*
*FORM zf_envia_emails .
*
*  DATA: lt_html TYPE html_table.
*
*  DATA: lv_werkst     TYPE werks_d,   "Werks Temp
*        lv_email      TYPE zemail255, "Email
*        lv_fechaemail TYPE char1,
*        lv_totalsaida TYPE ntgew,
*        lv_receivers  TYPE string.
*
*  READ TABLE tg_zlest0226 INTO wg_zlest0226 INDEX 1.
*
*  lv_fechaemail = ''.
*
*  CLEAR lt_html[].
*
*  CHECK  tg_zlest0039 IS NOT INITIAL.
*
*  SORT  tg_zlest0039 BY bukrs werks pontoentrega datasaida.
*
*  LOOP AT tg_225 INTO wg_225.
*    CLEAR lv_receivers.
*
*    LOOP AT tg_zlest0225 INTO wg_zlest0225
*       WHERE bukrs =  wg_225-bukrs
*         AND email =  wg_225-email.
*
*      IF NOT lv_receivers IS INITIAL.
*        CONCATENATE wg_zlest0225-email ';' lv_receivers INTO lv_receivers.
*      ELSE.
*        MOVE wg_zlest0225-email TO lv_receivers.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT tg_zlest0039 INTO wg_zlest0039
*        WHERE bukrs = wg_225-bukrs.
*
*      READ TABLE tg_j1bnflin INTO wg_j1bnflin
*      WITH KEY docnum = wg_zlest0039-docnum.
*
*      READ TABLE tg_j1bnfdoc INTO wg_j1bnfdoc
*      WITH KEY docnum = wg_zlest0039-docnum.
*
*      READ TABLE tg_zsdt_retlote INTO DATA(wg_zsdt_retlote)
*      WITH KEY docnum = wg_j1bnfdoc-docnum.
*
*      IF sy-subrc EQ 0.
*        READ TABLE tg_zsdt_export INTO DATA(wg_zsdt_export)
*        WITH KEY docnum = wg_zsdt_retlote-docnum_ret.
*        IF sy-subrc EQ 0.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
*
*      CLEAR wg_kna1.
*      READ TABLE tg_kna1 INTO wg_kna1
*      WITH KEY kunnr = wg_zlest0039-pontotransb.
*
*      READ TABLE tg_lfa1 INTO wg_lfa1
*      WITH KEY lifnr = wg_zlest0039-pontoentrega.
*
*      READ TABLE tg_t001w INTO wg_t001w
*      WITH KEY werks = wg_zlest0039-werks.
*
*
*      IF wg_zlest0039-werks EQ lv_werkst.
*        "Adiciona Linha
*        PERFORM f_add_html_line USING wg_j1bnfdoc
*                                      wg_j1bnflin
*                                      wg_zlest0039
*                                      wg_zlest0225
*                                      wg_kna1
*                                      wg_lfa1
*                                      wg_t001w
*
*                   CHANGING lt_html.
*        ADD wg_zlest0039-pesosaida TO lv_totalsaida.
*      ELSE.
*        PERFORM f_add_html_total USING lv_totalsaida
*                              CHANGING lt_html.
*
*        lv_totalsaida = 0.
*        "Adiciona Linha
*        PERFORM f_add_html_new_line CHANGING lt_html.
*
*        PERFORM f_add_html_line USING wg_j1bnfdoc
*                                      wg_j1bnflin
*                                      wg_zlest0039
*                                      wg_zlest0225
*                                      wg_kna1
*                                      wg_lfa1
*                                      wg_t001w
*                           CHANGING lt_html.
*
*        ADD wg_zlest0039-pesosaida TO lv_totalsaida.
*        MOVE wg_zlest0039-werks TO lv_werkst.
*      ENDIF.
*
*      CLEAR: wg_zsdt_retlote, wg_zsdt_export, wg_j1bnfdoc, wg_j1bnflin.
*
*    ENDLOOP.
*
*
*    "fecha e-mail
*    IF NOT lt_html IS INITIAL.
*      PERFORM f_add_html_close CHANGING lt_html.
*      PERFORM f_add_html_total USING lv_totalsaida
*                                 CHANGING lt_html.
*
**      PERFORM f_add_html_line_table CHANGING lt_html.
*
*
*      "Envia e-mail.
*      PERFORM f_send_email USING lv_receivers
*                                 'Acompanhamento de transito rodoviário'
*                                 lt_html.
*
*      CLEAR lt_html[].
*      CLEAR lv_receivers.
*      lv_totalsaida = 0.
*      lv_fechaemail = ''.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
*
*
*
**&---------------------------------------------------------------------*
**&      Form  F_ADD_HTML_LINE
**&---------------------------------------------------------------------*
*FORM f_add_html_line USING p_j1bnfdoc  TYPE j_1bnfdoc
*                           p_j1bnflin  TYPE j_1bnflin
*                           p_zlest0039 TYPE zlest0039
*                           p_zlest0225 TYPE zlest0225
*                           p_kna1 TYPE kna1
*                           p_lfa1 TYPE ty_lfa1
*                           p_wg_t001w TYPE t001w
*                  CHANGING p_html TYPE html_table.
*
*  DATA: lw_html    TYPE w3html,
*        lv_peso    TYPE ekpo-menge,
*        lv_menge   TYPE ekpo-menge,
*        lv_peso_   TYPE c LENGTH 20,
*        lv_meins   TYPE ekpo-meins,
*        lv_i_mein1 TYPE ekpo-meins,
*        lv_dias    TYPE i,
*        lv_dias2   TYPE char10,
*        lv_mat     TYPE char70,
*        lv_filial  TYPE char70,
*        lv_cod     TYPE char18,
*        lv_nfe     TYPE char18,
*        lv_dt      TYPE char10.
*
*  IF p_html IS INITIAL.
*    PERFORM f_add_html_ini CHANGING p_html.
*  ENDIF.
*
*
*  lw_html-line = '<tr style="font-Size: 13px">'.
*  APPEND lw_html TO p_html.
*
*  "Filial
*  CLEAR: lv_filial.
*  lv_filial = |{ p_zlest0039-werks } - { p_wg_t001w-name1 }|.
*  CONCATENATE '<td style="margin-left: 8px">' lv_filial '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  "Produto
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = p_j1bnflin-matnr
*    IMPORTING
*      output = lv_cod.
*
**  CONCATENATE lv_cod '-' p_j1bnflin-maktx INTO lv_mat.
**  CONDENSE lv_mat NO-GAPS.
*  CONCATENATE '<td align="center">' lv_cod '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*
*  "Produto
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = p_j1bnfdoc-nfenum
*    IMPORTING
*      output = lv_nfe.
*
*  "NFe
*  CONCATENATE '<td align="center">' lv_nfe '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  "Placa
*  CONCATENATE '<td align="center">' p_zlest0039-placa_cav '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  "DATA SAIDA
*  CONCATENATE p_zlest0039-datasaida+6(2)
*              p_zlest0039-datasaida+4(2)
*              p_zlest0039-datasaida+0(4)
*              INTO lv_dt SEPARATED BY '/'.
*
*  CONCATENATE '<td align="center">' lv_dt '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  "PESO SAIDA
*
*
*  MOVE p_zlest0039-pesosaida TO lv_peso.
*  WRITE lv_peso TO lv_peso_ DECIMALS 0.
*  CONDENSE lv_peso_ NO-GAPS.
*
*  lw_html-line = |<td align="center"> { lv_peso_ } </td>|.
**  CONCATENATE '<td>' lv_peso '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*
*  "DIAS TRANSITO
*  lv_dias = sy-datum - p_zlest0039-datasaida.
*  MOVE lv_dias TO lv_dias2.
*  CONDENSE lv_dias2 NO-GAPS.
*  CONCATENATE '<td align="center">' lv_dias2 '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  "Transbordo
*  CONCATENATE '<td style="margin-left: 8px">' p_kna1-name1 '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  "Porto
*  CONCATENATE '<td style="margin-left: 8px">' p_lfa1-name1 '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  CLEAR: v_observacao.
*  IF p_zlest0039-observacao IS NOT INITIAL.
*    v_observacao = p_zlest0039-observacao.
*  ELSE.
*    v_observacao = space.
*  ENDIF.
*
*  CONDENSE v_observacao NO-GAPS.
*
*  "Observação
*  CONCATENATE '<td style="margin-left: 8px">' v_observacao '</td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '</tr>'.
*  APPEND lw_html TO p_html.
*
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**&      Form  f_check_change_email
**&---------------------------------------------------------------------*
*FORM f_add_html_ini CHANGING p_html TYPE html_table.
*
*
*  DATA lw_html TYPE w3html.
**
*  lw_html-line = '<body>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<strong  style="color: #000; font-size: 11px, font-weight: bold, margin-buttom: 20px, font-family: Calibri" >'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = |Segue acompanhamento do volume em trânsito rodoviário. Favor verificar se há pendências da sua unidade e, se for RECUSA ou SINISTRO, gentileza abrir SRE para regularização do saldo com urgência.|.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '</strong>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<table border="1" cellspacing="1" cellpadding="0" style="border-collapse: collapse, margin-top: 20px">'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<tr bgcolor="#E0ECF8" style="color: #000; font-Size: 15px" align="center">'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 15%;"><strong>Filial</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 6%;"><strong>Produto</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 6%;"><strong>NFe</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 6%;"><strong>Placa</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 6%;"><strong>Data Saída</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 6%;"><strong>Peso Saída</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 7%;"><strong>Dia em Trânsito</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 15%;"><strong>Transbordo</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 15%;"><strong>Porto</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td style="width: 18%;"><strong>Observação</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '</tr>'.
*  APPEND lw_html TO p_html.
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**&      Form  f_add_html_new_line
**&---------------------------------------------------------------------*
*FORM f_add_html_new_line CHANGING p_html TYPE html_table.
*
*  DATA lw_html TYPE w3html.
*
*  IF p_html IS INITIAL.
*    PERFORM f_add_html_ini CHANGING p_html.
*    RETURN.
*  ENDIF.
*
**  lw_html-line = '<td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td>'.
**  APPEND lw_html TO p_html.
**
**  lw_html-line = '<tr><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td>'.
**  APPEND lw_html TO p_html.
**
**  lw_html-line = '<td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr>'.
**  APPEND lw_html TO p_html.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  f_check_change_email
**&---------------------------------------------------------------------*
*FORM f_add_html_total USING p_totalsaida TYPE ntgew
*                   CHANGING p_html TYPE html_table.
*
*  DATA: lw_html   TYPE w3html,
*        lv_tsaida TYPE ekpo-menge,
*        lv_total  TYPE c LENGTH 20.
*
*  IF p_html IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  APPEND INITIAL LINE TO p_html ASSIGNING FIELD-SYMBOL(<fs_html>).
*
*
*  lw_html-line = '<tr bgcolor="#E0ECF8" style="color: #000; font-Size: 15px;" align="center"><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td>'.
**  lw_html-line = '<tr>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td><strong>Total</strong></td>'.
*  APPEND lw_html TO p_html.
*
*  lv_tsaida = p_totalsaida.
*  WRITE lv_tsaida TO lv_total DECIMALS 0.
*  CONDENSE lv_total NO-GAPS.
*
*  "Valor
**  CLEAR: lv_tsaida.
**  lv_tsaida = p_totalsaida.
**  MOVE lv_tsaida TO lv_total.
*  CONDENSE lv_total NO-GAPS.
**
**  REPLACE '.' IN lv_total WITH ','.
*  CONCATENATE '<td><strong>' lv_total '</strong></td>' INTO lw_html-line.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '<td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr>'.
**  lw_html-line = '</tr>'.
*  APPEND lw_html TO p_html.
*
**  lw_html-line = '<div style="margin-top: 15px">'.
**  APPEND lw_html TO p_html.
**
**  lw_html-line = '</div>'.
**  APPEND lw_html TO p_html.
*
*  lw_html-line = '</body>'.
*  APPEND lw_html TO p_html.
*
*ENDFORM.
*
*
*
**&---------------------------------------------------------------------*
**&      Form  f_check_change_email
**&---------------------------------------------------------------------*
*FORM f_add_html_close CHANGING p_html TYPE html_table.
*
*  DATA: lw_html  TYPE w3html,
*        lv_total TYPE char17.
*
*  IF p_html IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  APPEND INITIAL LINE TO p_html ASSIGNING FIELD-SYMBOL(<fs_html>).
*
*  lw_html-line = '</table>'.
*  APPEND lw_html TO p_html.
*
**  lw_html-line = '</body>'.
**  APPEND lw_html TO p_html.
*
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_SEND_EMAIL
**&---------------------------------------------------------------------*
*
*
*
*FORM f_send_email  USING p_receivers TYPE string
*                         p_subject TYPE string
*                         p_body TYPE html_table.
*
*  DATA: lv_receivers    TYPE somlreci1_t,
*        lv_doc_dat      TYPE sodocchgi1,
*        lv_list         TYPE sopcklsti1_t,
*        lv_contents_txt TYPE html_table.
*
*  CLEAR: lv_receivers, lv_doc_dat, lv_list, lv_contents_txt.
*
*  APPEND INITIAL LINE TO lv_receivers ASSIGNING FIELD-SYMBOL(<fs_rec>).
*
*  <fs_rec>-receiver = p_receivers.
*  <fs_rec>-rec_type = 'U'.
*
*
*
*  APPEND INITIAL LINE TO lv_list ASSIGNING FIELD-SYMBOL(<fs_list>).
*
*  <fs_list>-head_start = 1.
*  <fs_list>-head_num = 0.
*  <fs_list>-body_start = 1.
*  <fs_list>-body_num = 99999.
*  <fs_list>-doc_type = 'HTM'.
*
*  lv_doc_dat-obj_name = p_subject.
*  lv_doc_dat-obj_descr = p_subject.
*  lv_doc_dat-no_change = 'X'.
*
*  lv_contents_txt = p_body.
*
*  "Enviar
*  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*    EXPORTING
*      document_data              = lv_doc_dat
*      put_in_outbox              = 'X'
*      commit_work                = 'X'
*    TABLES
*      packing_list               = lv_list
*      contents_txt               = lv_contents_txt
*      receivers                  = lv_receivers
*    EXCEPTIONS
*      too_many_receivers         = 1
*      document_not_sent          = 2
*      operation_no_authorization = 4
*      OTHERS                     = 99.
*
*  IF sy-subrc NE 0.
*
*  ENDIF.
*
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**&      Form  ZF_ENVIA_EMAILS_WERKS
**&---------------------------------------------------------------------*
*FORM zf_envia_emails_werks.
*
*
*  DATA: lt_html TYPE html_table,
*        lt_t225 TYPE TABLE OF zlest0225.
*
*  DATA: lv_werkst     TYPE werks_d,   "Werks Temp
*        lv_email      TYPE zemail255, "Email
*        lv_fechaemail TYPE char1,
*        lv_totalsaida TYPE ntgew,
*        lv_receivers  TYPE string.
*
*  READ TABLE tg_zlest0226 INTO wg_zlest0226 INDEX 1.
*
*  CHECK tg_zlest0039 IS NOT INITIAL.
*
*  lv_fechaemail = ''.
*
*  CLEAR: lt_html[].
*
*
*  LOOP AT tg_225_werks INTO wg_225_werks.
*    CLEAR lv_receivers.
*
*    LOOP AT tg_zlest0225 INTO wg_zlest0225
*       WHERE bukrs =  wg_225_werks-bukrs
*         AND werks =  wg_225_werks-werks.
*
*      IF NOT lv_receivers IS INITIAL.
*        CONCATENATE wg_zlest0225-email ';' lv_receivers INTO lv_receivers.
*      ELSE.
*        MOVE wg_zlest0225-email TO lv_receivers.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT tg_zlest0039 INTO wg_zlest0039
*        WHERE bukrs = wg_225_werks-bukrs
*          AND werks = wg_225_werks-werks.
*
*      READ TABLE tg_j1bnflin INTO wg_j1bnflin
*      WITH KEY docnum = wg_zlest0039-docnum.
*
*      READ TABLE tg_j1bnfdoc INTO wg_j1bnfdoc
*      WITH KEY docnum = wg_zlest0039-docnum.
*
*      READ TABLE tg_zsdt_retlote INTO DATA(wg_zsdt_retlote)
*      WITH KEY docnum = wg_j1bnfdoc-docnum.
*
*      IF sy-subrc EQ 0.
*        READ TABLE tg_zsdt_export INTO DATA(wg_zsdt_export)
*        WITH KEY docnum = wg_zsdt_retlote-docnum_ret.
*        IF sy-subrc EQ 0.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
*
*      CLEAR wg_kna1.
*      READ TABLE tg_kna1 INTO wg_kna1
*      WITH KEY kunnr = wg_zlest0039-pontotransb.
*
*      READ TABLE tg_lfa1 INTO wg_lfa1
*      WITH KEY lifnr = wg_zlest0039-pontoentrega.
*
*      READ TABLE tg_t001w INTO wg_t001w
*      WITH KEY werks = wg_zlest0039-werks.
*
*      IF wg_zlest0039-werks EQ lv_werkst.
*        "Adiciona Linha
*        PERFORM f_add_html_line USING wg_j1bnfdoc
*                                      wg_j1bnflin
*                                      wg_zlest0039
*                                      wg_zlest0225
*                                      wg_kna1
*                                      wg_lfa1
*                                      wg_t001w
*                   CHANGING lt_html.
*
*        ADD wg_zlest0039-pesosaida TO lv_totalsaida.
*      ELSE.
*        PERFORM f_add_html_total USING lv_totalsaida
*                              CHANGING lt_html.
*
*        lv_totalsaida = 0.
*        "Adiciona Linha
*        PERFORM f_add_html_new_line CHANGING lt_html.
*
*        PERFORM f_add_html_line USING wg_j1bnfdoc
*                                      wg_j1bnflin
*                                      wg_zlest0039
*                                      wg_zlest0225
*                                      wg_kna1
*                                      wg_lfa1
*                                      wg_t001w
*                           CHANGING lt_html.
*
*        ADD wg_zlest0039-pesosaida TO lv_totalsaida.
*        MOVE wg_zlest0039-werks TO lv_werkst.
*      ENDIF.
*
*      CLEAR: wg_zsdt_retlote, wg_zsdt_export, wg_j1bnfdoc, wg_j1bnflin.
*
*    ENDLOOP.
*    "fecha e-mail
*    IF NOT lt_html IS INITIAL.
*      PERFORM f_add_html_total USING lv_totalsaida
*                            CHANGING lt_html.
*
*      PERFORM f_add_html_close CHANGING lt_html.
*      "Envia e-mail.
*      PERFORM f_send_email USING lv_receivers
*                                 'Acompanhamento de transito rodoviário'
*                                 lt_html.
*
*      CLEAR lt_html[].
*      CLEAR lv_receivers.
*      lv_totalsaida = 0.
*      lv_fechaemail = ''.
*    ENDIF.
*
*  ENDLOOP.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  F_ADD_HTML_LINE_TABLE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LT_HTML  text
**      -->P_PERFORM  text
**      -->P_F_ADD_HTML_TOTAL  text
**      -->P_LV_TOTALSAIDA  text
**      <--P_LT_HTML  text
**----------------------------------------------------------------------*
*FORM f_add_html_line_table  CHANGING p_html TYPE html_table. .
*
*  DATA: lw_html     TYPE w3html.
*
*  lw_html-line = '</table>'.
*  APPEND lw_html TO p_html.
*
*  lw_html-line = '</body>'.
*  APPEND lw_html TO p_html.
*
*ENDFORM.
*
***********************************************************************HTML FIM
