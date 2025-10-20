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
REPORT zlesr0157.

TABLES: zlest0039,zlest0225.

TYPES:
  BEGIN OF ty_saida,
    cd_emp       TYPE bukrs, "Filial Cód
    nm_emp       TYPE butxt, "Filial Nome
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
  END OF ty_saida,

  BEGIN OF bukrs_werks,
    bukrs TYPE bukrs,
    werks TYPE werks,
  END OF bukrs_werks.




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
      tg_t001             TYPE TABLE OF t001,
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
      wg_t001w            LIKE LINE OF tg_t001w,
      wg_t001             LIKE LINE OF tg_t001.

DATA: r_exe           TYPE RANGE OF zexdiario,
      v_observacao    TYPE string,
      it_saida        TYPE STANDARD TABLE OF ty_saida WITH HEADER LINE,
      it_saida_aux    TYPE STANDARD TABLE OF ty_saida WITH HEADER LINE,
      aux_werks       TYPE STANDARD TABLE OF werks WITH HEADER LINE,
      aux_bukrs       TYPE STANDARD TABLE OF bukrs_fis WITH HEADER LINE,
      aux_bukrs_werks TYPE STANDARD TABLE OF bukrs_werks WITH HEADER LINE,
      it_filiais      TYPE STANDARD TABLE OF werks INITIAL SIZE 0,
      it_empresas     TYPE STANDARD TABLE OF bukrs_fis INITIAL SIZE 0,
      l_empresas      LIKE LINE OF it_empresas,
      lr_bukrs        TYPE RANGE OF bukrs_fis,
      lr_werks        TYPE RANGE OF werks,
      l_bukrs         LIKE LINE OF lr_bukrs,
      l_werks         LIKE LINE OF lr_werks,
      tipo            TYPE char4.


DATA dt_exec(12) TYPE c.
DATA dt_exec_sendfile(10) TYPE c.
DATA nm_sendfile TYPE string.
DATA nm_titulo TYPE so_obj_des.
DATA nm_subject TYPE sood-objdes.
DATA lv_size     TYPE i.
DATA     lr_filial TYPE RANGE OF werks.

DATA email_filial TYPE TABLE OF zlest0225 WITH HEADER LINE.
DATA email_send_fil TYPE TABLE OF zlest0225-email WITH HEADER LINE.

FIELD-SYMBOLS: <it_saida>.
DATA email TYPE string.

DATA: lt_mailsubject     TYPE sodocchgi1,
      lt_mailrecipientes TYPE STANDARD TABLE OF somlrec90 INITIAL SIZE 0,
      lt_mailtxt         TYPE STANDARD TABLE OF soli INITIAL SIZE 0.


DATA: lt_mail         TYPE STANDARD TABLE OF tvarvc,
      email_addresses TYPE adr6-smtp_addr.

DATA: total           TYPE i,
      total2          TYPE i,
      peso            TYPE i,
      total_emp       TYPE i,
      total_emp2      TYPE i,
      v_peso(15)      TYPE c,
      v_total(15)     TYPE c,
      v_total_emp(15) TYPE c.

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


FIELD-SYMBOLS: <email_filial>,
               <send_filais>,
               <_fil>.


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

  PERFORM monta_envia_email.

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


  SELECT *
     INTO TABLE @tg_zlest0039
    FROM zlest0039
    WHERE status     IN @r_status
     AND datatransb IN ('00000000', @space )
     AND ck_estornar_trans = ' '
     AND datachegada IN ('00000000', @space ) "Ajuste na seleção de dados / aoenning / 05-09-2023
     AND datasaida >= @p_dtsai
     AND bukrs IN ('0001','0050','0015')
      AND docnum IS NOT NULL
     AND datasaida <= @lv_datalimite.



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

  SORT tg_zlest0039 BY bukrs werks ASCENDING.


  SELECT * INTO TABLE tg_kna1
  FROM kna1
  FOR ALL ENTRIES IN tg_zlest0039
  WHERE kunnr EQ tg_zlest0039-pontotransb.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE tg_lfa1
    FROM lfa1
    FOR ALL ENTRIES IN tg_zlest0039
    WHERE lifnr  = tg_zlest0039-pontoentrega.



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

  LOOP AT tg_zlest0039 INTO wg_zlest0039.
*      WHERE bukrs = wg_225-bukrs.

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

    it_saida-cd_emp = wg_zlest0039-bukrs."Empresa Cód
    SELECT SINGLE butxt FROM t001 INTO it_saida-nm_emp WHERE bukrs = wg_zlest0039-bukrs.
    it_saida-cd_fil = wg_zlest0039-werks."Filial Cód
    SELECT SINGLE name1 FROM t001w INTO it_saida-nm_fil WHERE werks = wg_zlest0039-werks. "Filial Nome
    it_saida-cd_placa = wg_zlest0039-placa_cav. "Placa
    it_saida-dt_saida = |{ wg_zlest0039-datasaida+6(2) }/{ wg_zlest0039-datasaida+4(2) }/{ wg_zlest0039-datasaida+0(4) }|. "Data Saida
    CONDENSE it_saida-dt_saida NO-GAPS.
    it_saida-dias_dif = sy-datum - wg_zlest0039-datasaida. "DIAS TRANSITO
    CONDENSE it_saida-dias_dif NO-GAPS.
    SELECT SINGLE name1 FROM kna1 INTO it_saida-nm_trans WHERE kunnr = wg_zlest0039-pontotransb. "Transbordo
    SELECT SINGLE name1 FROM lfa1 INTO it_saida-nm_port WHERE lifnr = wg_zlest0039-pontoentrega. "Porto

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
    CLEAR: wg_zsdt_retlote, wg_zsdt_export, wg_j1bnfdoc, wg_j1bnflin, wg_zlest0039, tg_t001w, tg_lfa1, tg_kna1, wg_j1bnfdoc, wg_j1bnflin.
  ENDLOOP.

  SORT it_saida BY cd_emp cd_fil ASCENDING.

ENDFORM.


FORM monta_envia_email. "PSA 15/09/2023

**********************************************************************
* Se For Senmanal ou Diaria

  FREE: tg_zlest0225.

  IF p_exe IS NOT INITIAL. "Diario
    SELECT * FROM zlest0225 WHERE exediario = @p_exe INTO TABLE @tg_zlest0225.
  ELSE. "Semanal
    SELECT * FROM zlest0225 WHERE exediario <> 'X' INTO TABLE @tg_zlest0225.
  ENDIF.

**********************************************************************
* Resenva As Empresas e Unidades da it_saida

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<empfil>).
    aux_bukrs_werks-bukrs = <empfil>-cd_emp.
    aux_bukrs_werks-werks = <empfil>-cd_fil.
    APPEND aux_bukrs_werks.
  ENDLOOP.

**********************************************************************
* Separa os e-mails para envio

  TYPES: BEGIN OF ty_empubimail,
           bukrs TYPE bukrs,
           werks TYPE werks,
           email TYPE zlest0225-email,
         END OF ty_empubimail,

         BEGIN OF ty_receiver,
           remetente TYPE zlest0225-email,
         END OF ty_receiver.

  DATA remetentes TYPE STANDARD TABLE OF ty_receiver WITH HEADER LINE.
  DATA empunimail TYPE TABLE OF ty_empubimail WITH HEADER LINE.

  DELETE ADJACENT DUPLICATES FROM aux_bukrs_werks.

  LOOP AT  tg_zlest0225 ASSIGNING FIELD-SYMBOL(<remetente>).
    remetentes = <remetente>-email.
    APPEND remetentes.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM remetentes.
**********************************************************************

  LOOP AT remetentes ASSIGNING FIELD-SYMBOL(<email>).

    "Texto do Corpo E-mail
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
    APPEND 'Senhores(as),<br>Segue acompanhamento do volume em trânsito rodoviário. Por favor verificar se existe alguma pendências da sua filial e,  <br>' TO html.
    APPEND 'caso seja alguma carga RECUSADA ou SINISTRADA, gentileza prosseguir com a abertura do SRE para regularização do saldo com urgência.</strong><br></br>' TO html.
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

**********************************************************************
* Reserva as empresas e Unidades Cadastradas para o e-mail na Zles0208

    DATA(cp_tg_zlest0225) = tg_zlest0225.

    DELETE cp_tg_zlest0225 WHERE email <> <email>-remetente.

**********************************************************************
* Conforme as Empresas Cadastrada e Unidadades é definado o range para envio das mesmas!

    LOOP AT cp_tg_zlest0225 ASSIGNING FIELD-SYMBOL(<valid_emp>).
      IF <valid_emp>-bukrs IS NOT INITIAL AND <valid_emp>-werks IS NOT INITIAL.
        LOOP AT  aux_bukrs_werks ASSIGNING FIELD-SYMBOL(<get_empresas>) WHERE bukrs = <valid_emp>-bukrs.
          l_bukrs-sign = 'I'.
          l_bukrs-option = 'EQ'.
          l_bukrs-low = <get_empresas>-bukrs.
          "l_werks-high = 'LH'.
          APPEND l_bukrs TO lr_bukrs.
        ENDLOOP.
        LOOP AT  aux_bukrs_werks ASSIGNING FIELD-SYMBOL(<get_filiais>) WHERE werks = <valid_emp>-werks.
          l_werks-sign = 'I'.
          l_werks-option = 'EQ'.
          l_werks-low = <get_filiais>-werks.
          "l_werks-high = 'LH'.
          APPEND l_werks TO lr_werks.
        ENDLOOP.
      ELSE.
        LOOP AT aux_bukrs_werks ASSIGNING FIELD-SYMBOL(<full>) WHERE bukrs = <valid_emp>-bukrs.
          l_bukrs-sign = 'I'.
          l_bukrs-option = 'EQ'.
          l_bukrs-low = <full>-bukrs.
          "l_werks-high = 'LH'.
          APPEND l_bukrs TO lr_bukrs.
          l_werks-sign = 'I'.
          l_werks-option = 'EQ'.
          l_werks-low = <full>-werks.
          "l_werks-high = 'LH'.
          APPEND l_werks TO lr_werks.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lr_bukrs.
    DELETE ADJACENT DUPLICATES FROM lr_werks.

    SORT lr_bukrs ASCENDING.
    SORT lr_werks ASCENDING.

**********************************************************************
* Monta o HTML por e-mail e agrupado por empresa e filial.

    LOOP AT aux_bukrs_werks ASSIGNING FIELD-SYMBOL(<total>) WHERE werks IN lr_werks.

      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<emp_fil>) WHERE cd_fil = <total>-werks.    "IN lr_werks.
        APPEND '<tr>' TO html.
        CONCATENATE '<td>' <emp_fil>-cd_fil   '</td>' INTO html.    APPEND html.
        CONCATENATE '<td>' <emp_fil>-nm_fil   '</td>' INTO html.    APPEND html.
        DATA prod TYPE string.
        PACK <emp_fil>-cd_prod TO prod.
        CONCATENATE '<td>' prod  '</td>' INTO html.    APPEND html.
        DATA nfe TYPE string.
        nfe = <emp_fil>-cd_nfe.
        CONCATENATE '<td>' nfe   '</td>' INTO html.    APPEND html.
        CONCATENATE '<td>' <emp_fil>-cd_placa '</td>' INTO html.    APPEND html.
        CONCATENATE '<td>' <emp_fil>-dt_saida '</td>' INTO html.    APPEND html.
        peso = <emp_fil>-kg_saida.
        WRITE  peso TO v_peso EXPONENT 0.
        CONCATENATE '<td style="text-align: center; vertical-align:middle !important" >' v_peso '</td>' INTO html.    APPEND html.
        CONCATENATE '<td style="text-align: center; vertical-align:middle !important" >' <emp_fil>-dias_dif '</td>' INTO html.    APPEND html.
        CONCATENATE '<td>' <emp_fil>-nm_trans '</td>' INTO html.    APPEND html.
        CONCATENATE '<td>' <emp_fil>-nm_port  '</td>' INTO html.    APPEND html.
        CONCATENATE '<td>' <emp_fil>-nm_obs   '</td>' INTO html.    APPEND html.
        APPEND '</tr>' TO html.

        total2 = <emp_fil>-kg_saida.
        total = total2 + total .
      ENDLOOP.

      APPEND '<tr>' TO html.
      CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
      WRITE  total TO v_total EXPONENT 0.
      CONCATENATE '<td style="text-align: center; vertical-align:middle !important" >' v_total '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
      CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
      APPEND '</tr>' TO html.

      CLEAR: total2,total.

    ENDLOOP.

    APPEND '</table>' TO html.
    APPEND '</body>' TO html.
    APPEND '</html>' TO html.

    APPEND LINES OF html TO it_contents.

    CLEAR: prod,total2,total,peso,nfe,lr_werks,lr_bukrs,cp_tg_zlest0225.
**********************************************************************
* Inicia o Envio do E-mail

    email_addresses = <email>-remetente.
    TRANSLATE email_addresses TO LOWER CASE.
    CONDENSE email_addresses NO-GAPS.

    TRY.

        send_request = cl_bcs=>create_persistent( ).
        document = cl_document_bcs=>create_document(
          i_type    = 'HTM'
          i_text    = it_contents
          i_subject = |{ nm_sendfile }| ). "Título do e-mail

        send_request->set_document( document ).

        recipient = cl_cam_address_bcs=>create_internet_address( email_addresses ).
        send_request->add_recipient( recipient ).

        DATA(sent_to_all) = send_request->send( i_with_error_screen = 'X' ).
      CATCH cx_root.
    ENDTRY.
    COMMIT WORK.
**********************************************************************
* Limpa as variaveis

    CLEAR: html,it_contents,email_addresses,send_request,document,recipient,sent_to_all.
    FREE: html,it_contents.

  ENDLOOP.
  "Fim do Loop de envio de E-mail!
**********************************************************************
*Limpa as Tabelas
  FREE: remetentes,empunimail,aux_bukrs_werks,cp_tg_zlest0225,tg_zlest0225,l_bukrs,l_werks,it_saida,html,it_contents.

ENDFORM.





**********************************************************************
* versão 2.0
*
*TABLES: zlest0039,t001,t001w.
*
*TYPES: BEGIN OF ty_saida,
*         full        TYPE string,
*         parc        TYPE string,
*         status      TYPE char2,
*         doc         TYPE string,
*         cd_emp      TYPE string,
*         nm_emp      TYPE string,
*         cd_fil      TYPE t001w-werks,
*         nm_fil      TYPE string,
*         cd_mat      TYPE makt-matnr,
*         nm_mat      TYPE string,
*         nfe         TYPE CHAR6,
*         dias        TYPE int1,
*         placa       TYPE char10,
*         dt_saida    TYPE dats,
*         peso  TYPE INT8,
*         cd_pentrega TYPE char10,
*         nm_pentrega TYPE string,
*         cd_transb   TYPE char10,
*         nm_transb   TYPE string,
*         obs         TYPE string,
*       END OF ty_saida.
*
*DATA: BEGIN OF l_saida,
*        full        TYPE string,
*        parc        TYPE string,
*        status      TYPE char2,
*        doc         TYPE string,
*        cd_emp      TYPE string,
*        nm_emp      TYPE string,
*        cd_fil      TYPE t001w-werks,
*        nm_fil      TYPE string,
*        cd_mat      TYPE makt-matnr,
*        nm_mat      TYPE string,
*        nfe         TYPE CHAR6,
*        dias        TYPE int1,
*        placa       TYPE char10,
*        dt_saida    TYPE dats,
*        PESO  TYPE INT8,
*        cd_pentrega TYPE char10,
*        nm_pentrega TYPE string,
*        cd_transb   TYPE char10,
*        nm_transb   TYPE string,
*        obs         TYPE string,
*      END OF l_saida.
*
*DATA it_saida TYPE STANDARD TABLE OF ty_saida.
*
*DATA: p_dt_saida    TYPE  sy-datum,
*      lv_datalimite TYPE sy-datum.
*
*DATA: tg_zlest0226        TYPE TABLE OF zlest0226.
*
*DATA: r_date   TYPE RANGE OF sy-datum,
*      r_status TYPE RANGE OF zstatus_comp.
*
*
*DATA: r_docnum TYPE RANGE OF zlest0039-docnum.
*
*
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002  .
*  PARAMETERS: p_dtsai TYPE zlest0039-datasaida.
*  SELECT-OPTIONS: "p_emp FOR t001-bukrs,
*  p_fil FOR t001w-werks.
*SELECTION-SCREEN END OF BLOCK b1.
*
*
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003  .
*      PARAMETER: p_smail AS CHECKBOX DEFAULT 'X' USER-COMMAND SMAIL,
*      p_ALV AS CHECKBOX DEFAULT '' ."USER-COMMAND ALV.
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN POSITION 1 .
*    PARAMETER: p_exe AS CHECKBOX DEFAULT '' USER-COMMAND abc.
*    SELECTION-SCREEN COMMENT 05(20) TEXT-005 FOR FIELD p_exe.
*  SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK b2.
*
*
*DATA c_vazio TYPE char1.
*DATA dt_vazio TYPE dats.
*DATA dt_vazio0 TYPE dats.
*DATA dt_hj TYPE zlest0039-datasaida.
*c_vazio = ''.
*dt_vazio = ''.
*dt_vazio0 = '00000000'.
*dt_hj = sy-datum.
*
*SELECT SINGLE * INTO @DATA(wg_zlest0226)
*FROM zlest0226.
*
*CHECK wg_zlest0226 IS NOT INITIAL.
*
*lv_datalimite = sy-datum - wg_zlest0226-dias.
*r_date = VALUE #( ( sign = 'I' option = 'BT' low = p_dtsai high = lv_datalimite ) ).
*
*APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ET' ) TO r_status.
*APPEND VALUE #( sign = 'I' option = 'EQ' low = 'L1' ) TO r_status.
*
*DATA: rg_cfop TYPE RANGE OF j_1bcfop.
*DATA: w_cfop LIKE LINE OF rg_cfop.
*DATA          tg_tvarvc           TYPE TABLE OF tvarvc.
*DATA   wg_tvarvc           LIKE LINE OF tg_tvarvc.
*
*CONSTANTS: c_name TYPE rvari_vnam VALUE 'Z_ZCFOP_ZLES0050'.
*
*SELECT * INTO TABLE tg_tvarvc
*  FROM tvarvc
*  WHERE name = c_name.
*IF sy-subrc EQ 0.
*  LOOP AT tg_tvarvc INTO wg_tvarvc.
*    w_cfop-sign = 'I'.
*    w_cfop-option = 'EQ'.
*    w_cfop-low = wg_tvarvc-low.
*
*    "retirar a barra.
*    REPLACE ALL OCCURRENCES OF '/' IN w_cfop-low WITH ' '.
*
*    APPEND w_cfop TO rg_cfop.
*  ENDLOOP.
*ENDIF.
*
*WITH
*+dados AS ( SELECT
*a~status AS status,
*a~docnum AS doc,
*a~bukrs AS cd_emp,
*d~butxt AS nm_emp,
*a~werks AS cd_fil,
*e~name1 AS nm_fil,
*a~matnr AS cd_mat,
*b~maktx AS nm_mat,
*c~nfenum AS nfe,
*CAST( dats_days_between( a~datasaida , @dt_hj ) AS INT1 ) AS dias,
*a~placa_cav AS placa,
*a~datasaida AS dt_saida,
*a~pesosaida AS peso,
*a~pontoentrega AS cd_pentrega,
*g~name1 AS nm_pentrega,
*a~pontotransb AS cd_transb,
*f~name1 AS nm_transb,
*a~observacao AS obs
*FROM zlest0039 AS a
*LEFT JOIN j_1bnflin AS b ON b~docnum = a~docnum
*LEFT JOIN j_1bnfdoc AS c ON c~docnum = a~docnum
*LEFT JOIN t001 AS d ON d~bukrs = a~bukrs
*LEFT JOIN t001w AS e ON e~werks = a~werks
*LEFT JOIN kna1 AS f ON f~kunnr = a~pontotransb
*LEFT JOIN lfa1 AS g ON g~lifnr = a~pontoentrega
*WHERE 1 = 1
*AND a~status IN @r_status
*AND a~datatransb IN ( @dt_vazio0 , @dt_vazio )
*AND a~ck_estornar_trans = @c_vazio
*AND a~datachegada IN ( @dt_vazio0 , @dt_vazio )
*AND a~datasaida >= @p_dtsai
*AND a~datasaida <= @lv_datalimite
*AND a~werks IN @p_fil
*AND a~bukrs IN ('0001','0050','0015') "@p_emp
*AND a~docnum IS NOT NULL
*AND b~cfop IN @rg_cfop ),
*+full AS ( SELECT a~bukrs,STRING_AGG( a~email,';' ) AS full FROM zlest0225 AS a WHERE a~bukrs NOT IN ('0000') GROUP BY a~bukrs ),
*+parc AS ( SELECT a~werks,STRING_AGG( a~email,';' ) AS parc FROM zlest0225 AS a WHERE a~werks NOT IN ('0000') GROUP BY a~werks )
*
*SELECT
*a~status,
*a~doc,
*a~cd_emp,
*a~nm_emp,
*a~cd_fil,
*a~nm_fil,
*a~cd_mat,
*a~nm_mat,
*a~nfe,
*a~dias,
*a~placa,
*a~dt_saida,
*a~peso,
*a~cd_pentrega,
*a~nm_pentrega,
*a~cd_transb,
*a~nm_transb,
*a~obs,
*  b~full,
*  c~parc
*  FROM +dados AS a
*LEFT JOIN +full AS b ON a~cd_emp = b~bukrs
*LEFT JOIN +parc AS c ON a~cd_fil = c~werks
*"and DATS_DAYS_BETWEEN( a~datasaida, @dt_hj ) > 7
*INTO TABLE @DATA(it_dados).
*
*LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<it_dados>).
*
*  l_saida-cd_emp = <it_dados>-cd_emp.
*  l_saida-nm_emp = <it_dados>-nm_emp.
*  l_saida-cd_fil = <it_dados>-cd_fil.
*  l_saida-nm_fil = <it_dados>-nm_fil.
*  l_saida-status = <it_dados>-status.
*  l_saida-doc = <it_dados>-doc.
*  pack <it_dados>-cd_mat to l_saida-cd_mat.
*  l_saida-nm_mat = <it_dados>-nm_mat.
*
*  pack <it_dados>-nfe to l_saida-nfe.
*
*  l_saida-dias = <it_dados>-dias.
*  l_saida-placa = <it_dados>-placa.
*  l_saida-dt_saida = <it_dados>-dt_saida.
*  l_saida-peso = <it_dados>-peso.
*  l_saida-cd_pentrega = <it_dados>-cd_pentrega.
*  l_saida-nm_pentrega = <it_dados>-nm_pentrega.
*  l_saida-cd_transb = <it_dados>-cd_transb.
*  l_saida-nm_transb = <it_dados>-nm_transb.
*  l_saida-obs = <it_dados>-obs.
*  l_saida-full = <it_dados>-full.
*  l_saida-parc = <it_dados>-parc.
*
*  APPEND l_saida TO it_saida.
*
*  CLEAR: l_saida.
*
*ENDLOOP.
*
*SORT it_saida BY cd_emp cd_fil ASCENDING.
*
*IF p_ALV = 'X'.
*PERFORM ALV.
*else.
*  PERFORM monta_email.
*ENDIF.
*
*
*
*
*
*FORM ALV.
*
*DATA: go_alv        TYPE REF TO cl_salv_table.
*DATA: lr_columns    TYPE REF TO cl_salv_columns_table.
*DATA: lr_column     TYPE REF TO cl_salv_column_table.
*DATA: lr_functions  TYPE REF TO cl_salv_functions_list.
*DATA: gr_display    TYPE REF TO cl_salv_display_settings.
*DATA: gr_selections TYPE REF TO cl_salv_selections.
*DATA: lv_rows        TYPE string.
*DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid. " Variables for header
*DATA:      lo_layout_logo TYPE REF TO cl_salv_form_layout_logo.
*DATA:     lo_content     TYPE REF TO cl_salv_form_element.
*DATA:    lv_title       TYPE string.
*
*
***ALV Class
*TRY.
*    cl_salv_table=>factory(
*      IMPORTING
*        r_salv_table = go_alv
*      CHANGING
*        t_table      = it_SAIDA ). "Internal Table
*
*  CATCH cx_salv_msg.
*ENDTRY.
*
***Enable function buttons
*lr_functions = go_alv->get_functions( ).
*lr_functions->set_all( 'X' ).
*
***Optimize Column
*lr_columns = go_alv->get_columns( ).
*lr_columns->set_optimize( 'X' ).
*
***Enable Zebra style
*gr_display = go_alv->get_display_settings( ).
*gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*
** Create header
*DESCRIBE TABLE it_saida LINES lv_rows.
*CONCATENATE 'Total de Linhas: ' lv_rows INTO lv_title SEPARATED BY space.
*
*CREATE OBJECT lo_grid.
*CREATE OBJECT lo_layout_logo.
*lo_grid->create_label( row = 1 column = 1 text = lv_title tooltip = lv_title ).
*lo_layout_logo->set_left_content( lo_grid ).
*lo_content = lo_layout_logo.
*go_alv->set_top_of_list( lo_content ).
*
** Apply zebra style to lv_rows
*gr_display = go_alv->get_display_settings( ).
*gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*
** Enable cell selection mode
*gr_selections = go_alv->get_selections( ).
*gr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
*
*
*TRY.
*    lr_column ?= lr_columns->get_column( 'FULL' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Envio Completo' ).
*    lr_column->set_medium_text( 'Env. Comp.' ).
*    lr_column->set_short_text( 'EnvC.' ).
*
*    lr_column ?= lr_columns->get_column( 'PARC' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Envio Parcial' ).
*    lr_column->set_medium_text( 'Env. Parc.' ).
*    lr_column->set_short_text( 'EnvParc.' ).
*
*    lr_column ?= lr_columns->get_column( 'CD_EMP' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Emp. Cód.' ).
*    lr_column->set_medium_text( 'Emp. Cód' ).
*    lr_column->set_short_text( 'EmpCod' ).
*
*    lr_column ?= lr_columns->get_column( 'NM_EMP' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Emp. Nome' ).
*    lr_column->set_medium_text( 'Emp. Nome' ).
*    lr_column->set_short_text( 'EmpNome' ).
*
*    lr_column ?= lr_columns->get_column( 'CD_FIL' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Fil. Cód.' ).
*    lr_column->set_medium_text( 'Fil. Cód' ).
*    lr_column->set_short_text( 'FilCod' ).
*
*    lr_column ?= lr_columns->get_column( 'NM_FIL' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Fil. Nome' ).
*    lr_column->set_medium_text( 'Fil. Nome' ).
*    lr_column->set_short_text( 'FilNome' ).
*
*    lr_column ?= lr_columns->get_column( 'CD_MAT' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Mat. Cód.' ).
*    lr_column->set_medium_text( 'Mat. Cód.' ).
*    lr_column->set_short_text( 'MatCod' ).
*
*    lr_column ?= lr_columns->get_column( 'NM_MAT' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Mat. Nome' ).
*    lr_column->set_medium_text( 'Mat. Nome' ).
*    lr_column->set_short_text( 'MatNom' ).
*
*
*    lr_column ?= lr_columns->get_column( 'NFE' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'NFE' ).
*    lr_column->set_medium_text( 'NFE' ).
*    lr_column->set_short_text( 'NFE' ).
*
*    lr_column ?= lr_columns->get_column( 'DIAS' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Dias' ).
*    lr_column->set_medium_text( 'Dias' ).
*    lr_column->set_short_text( 'Dias' ).
*
*    lr_column ?= lr_columns->get_column( 'PLACA' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Placa' ).
*    lr_column->set_medium_text( 'Placa' ).
*    lr_column->set_short_text( 'Placa' ).
*
*    lr_column ?= lr_columns->get_column( 'DT_SAIDA' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Saida Data' ).
*    lr_column->set_medium_text( 'Saida Dt.' ).
*    lr_column->set_short_text( 'SaidaDt' ).
*
*
*    lr_column ?= lr_columns->get_column( 'PESO' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'PESO SAIDA' ).
*    lr_column->set_medium_text( 'PESO' ).
*    lr_column->set_short_text( 'PESO' ).
*
*    lr_column ?= lr_columns->get_column( 'CD_PENTREGA' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Porto Cód.' ).
*    lr_column->set_medium_text( 'Porto Cód.' ).
*    lr_column->set_short_text( 'PortoCd.' ).
*
*
*    lr_column ?= lr_columns->get_column( 'NM_PENTREGA' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Porto Nome' ).
*    lr_column->set_medium_text( 'Porto Nome' ).
*    lr_column->set_short_text( 'PortoNom' ).
*
*    lr_column ?= lr_columns->get_column( 'CD_TRANSB' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Transb. Cód.' ).
*    lr_column->set_medium_text( 'Transb. Cód.' ).
*    lr_column->set_short_text( 'TransbCod' ).
*
*    lr_column ?= lr_columns->get_column( 'NM_TRANSB' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Transb. Nome' ).
*    lr_column->set_medium_text( 'Transb. Nome' ).
*    lr_column->set_short_text( 'TransbNom' ).
*
*    lr_column ?= lr_columns->get_column( 'OBS' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Observação' ).
*    lr_column->set_medium_text( 'Observação' ).
*    lr_column->set_short_text( 'Obs' ).
*
*    lr_column ?= lr_columns->get_column( 'STATUS' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Status' ).
*    lr_column->set_medium_text( 'Status' ).
*    lr_column->set_short_text( 'ST' ).
*
*    lr_column ?= lr_columns->get_column( 'DOC' ). " Find the 'MAKTX' column ans change attributes
*    lr_column->set_visible( if_salv_c_bool_sap=>true ).
*    lr_column->set_long_text( 'Documento' ).
*    lr_column->set_medium_text( 'Docum.' ).
*    lr_column->set_short_text( 'Doc' ).
*
*  CATCH cx_salv_not_found.
*  CATCH cx_salv_existing.
*  CATCH cx_salv_data_error.
*ENDTRY.
*
*go_alv->get_columns( )->set_column_position( columnname = 'CD_EMP' position = 1 ).
*go_alv->get_columns( )->set_column_position( columnname = 'NM_EMP' position = 2 ).
*go_alv->get_columns( )->set_column_position( columnname = 'CD_FIL' position = 3 ).
*go_alv->get_columns( )->set_column_position( columnname = 'NM_FIL' position = 4 ).
*go_alv->get_columns( )->set_column_position( columnname = 'CD_MAT' position = 5 ).
*go_alv->get_columns( )->set_column_position( columnname = 'NM_MAT' position = 6 ).
*go_alv->get_columns( )->set_column_position( columnname = 'NFE' position = 7 ).
*go_alv->get_columns( )->set_column_position( columnname = 'DIAS' position = 8 ).
*go_alv->get_columns( )->set_column_position( columnname = 'PLACA' position = 9 ).
*go_alv->get_columns( )->set_column_position( columnname = 'DT_SAIDA' position = 10 ).
*go_alv->get_columns( )->set_column_position( columnname = 'PESO' position = 11 ).
*go_alv->get_columns( )->set_column_position( columnname = 'CD_PENTREGA' position = 12 ).
*go_alv->get_columns( )->set_column_position( columnname = 'NM_PENTREGA' position = 13 ).
*go_alv->get_columns( )->set_column_position( columnname = 'CD_TRANSB' position = 14 ).
*go_alv->get_columns( )->set_column_position( columnname = 'NM_TRANSB' position = 15 ).
*go_alv->get_columns( )->set_column_position( columnname = 'OBS' position = 16 ).
*go_alv->get_columns( )->set_column_position( columnname = 'DOC' position = 17 ).
*go_alv->get_columns( )->set_column_position( columnname = 'FULL' position = 18 ).
*go_alv->get_columns( )->set_column_position( columnname = 'PARC' position = 19 ).
*
***Display ALV
*go_alv->display( ).
*ENDFORM.
*
*
*FORM monta_email.
*
*  types: BEGIN OF ty_full,
*    bukrs TYPE ZLEST0225-bukrs,
*    "email type string,
*    END OF ty_full,
*
*    BEGIN OF ty_parc,
*     werks TYPE ZLEST0225-werks,
*    "email type string,
*      END OF ty_parc.
*
*    data: it_full TYPE STANDARD TABLE OF ty_full,
*          wa_full TYPE ty_full,
*          it_parc TYPE STANDARD TABLE OF ty_parc,
*          wa_parc TYPE ty_parc.
*
*
*LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<emp_fil>).
*
*IF <emp_fil>-full is NOT INITIAL.
*  wa_full-bukrs = <emp_fil>-cd_emp.
*  APPEND wa_full to it_full.
*ENDIF.
*
*IF <emp_fil>-parc is NOT INITIAL.
*  wa_parc-werks = <emp_fil>-cd_fil.
*  APPEND wa_parc to it_parc.
*ENDIF.
*
*  ENDLOOP.
*
*  delete ADJACENT DUPLICATES FROM it_parc.
*  delete ADJACENT DUPLICATES FROM it_full.
*
*  types: BEGIN OF ty_send_full,
*    bukrs type ZLEST0225-bukrs,
*    email type ZLEST0225-email,
*    END OF ty_send_full,
*
*  BEGIN OF ty_send_parc,
*      werks type ZLEST0225-werks,
*    email type ZLEST0225-email,
*    END OF ty_send_parc.
*
*    data: send_empresa TYPE STANDARD TABLE OF ty_send_full,
*          send_filial TYPE STANDARD TABLE OF ty_send_parc.
*
*    select bukrs,email
*    from ZLEST0225
*    FOR ALL ENTRIES IN @it_full
*      where bukrs = @it_full-bukrs
*    INTO TABLE @send_empresa.
*
*
*DATA : it_contents TYPE STANDARD TABLE OF solisti1,
*       html        TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE.
*DATA lt_att_head TYPE soli_tab.
*DATA xhtml_string TYPE xstring.
*DATA t_hex TYPE solix_tab.
*
*
*  LOOP AT send_empresa ASSIGNING FIELD-SYMBOL(<SEND_EMP>).
*
*      APPEND '<html>' TO html.
*  APPEND '<style>' TO html.
*  APPEND 'table { font-size: 14px; font-family: arial, sans-serif; border: 0px solid #ffffff; border-collapse: collapse; text-align: center;}' TO html.
*  APPEND 'td{ font-size: 14px; font-family: arial, sans-serif; border: 1px solid #dddddd; text-align: left;}' TO html.
*  APPEND 'th{ font-size: 14px; font-family: arial, sans-serif; border: 1px solid #dddddd; text-align: center;}' TO html.
*  APPEND '.bg {background-color: #C8E6C9;}' TO html.
*  APPEND '</style>' TO html.
*  APPEND '</head>' TO html.
*  APPEND '<body>' TO html.
*  APPEND '<strong  style="color: #000; font-size: 14px, font-weight: bold, margin-buttom: 11px, font-family: Calibri" >' TO html.
*  APPEND 'Senhores(as),<br>Segue acompanhamento do volume em trânsito rodoviário. Favor verificar se há pendências da sua unidade e, se <br>' TO html.
*  APPEND 'for RECUSA ou SINISTRO, gentileza abrir SRE para regularização do saldo com urgência.</strong><br></br>' TO html.
*  APPEND '<table style="width:0%">' TO html.
*  APPEND '<tr bgcolor="#E0ECF8" style="color: #000; font-Size: 14px" align="center">'TO html.
*  APPEND '<th><strong>Filial</strong></th>' TO html.
*  APPEND '<th><strong>Descrição</strong></th>' TO html.
*  APPEND '<th><strong>Produto</strong></th>' TO html.
*  APPEND '<th><strong>NFe</strong></th>' TO html.
*  APPEND '<th><strong>Placa</strong></th>' TO html.
*  APPEND '<th><strong>Data Saída</strong></th>' TO html.
*  APPEND '<th><strong>Peso Saída</strong></th>' TO html.
*  APPEND '<th><strong>Dias</strong></th>' TO html.
*  APPEND '<th><strong>Transbordo</strong></th>' TO html.
*  APPEND '<th><strong>Porto</strong></th>' TO html.
*  APPEND '<th><strong>Observação</strong></th>' TO html.
*  APPEND '</tr>' TO html.
*
*    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<EMPRESA>) WHERE cd_emp = <SEND_EMP>-bukrs.
*      APPEND '<tr>' TO html.
*      CONCATENATE '<td>' <EMPRESA>-cd_fil   '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-nm_fil   '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-cd_mat  '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-nfe   '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-placa '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-dt_saida '</td>' INTO html.    APPEND html.
*      DATA(TOTAL) = <EMPRESA>-peso.
*      data d TYPE string.
*      data p TYPE string.
*      d = <EMPRESA>-dias.
*      p = TOTAL.
*      CONCATENATE '<td style="text-align: center; vertical-align:middle !important" >' p '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td style="text-align: center; vertical-align:middle !important" >' d '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-nm_transb '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-nm_pentrega  '</td>' INTO html.    APPEND html.
*      CONCATENATE '<td>' <EMPRESA>-obs   '</td>' INTO html.    APPEND html.
*      APPEND '</tr>' TO html.
*
*    ENDLOOP.
*
*    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<SEND_EMP_total>) WHERE cd_emp = <SEND_EMP>-bukrs.
*      total = <SEND_EMP_total>-peso + total .
*
*    ENDLOOP.
*      DATA V_TOTAL TYPE STRING.
*      V_TOTAL = total .
***********************************************************************
**. Total Agrpador por Empresa
*    APPEND '<tr>' TO html.
*    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>'  space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>' V_TOTAL  '</td>' INTO html.    APPEND html. "total
*    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
*    CONCATENATE '<td>' space  '</td>' INTO html.    APPEND html.
*    APPEND '</tr>' TO html.
***********************************************************************
*
*      APPEND '</table>' TO html.
*  APPEND '</body>' TO html.
*  APPEND '</html>' TO html.
*
*  APPEND LINES OF html TO it_contents.
*  ENDLOOP.
*
*  "CLEAR: peso, total, v_total,v_peso.
*
*
*
*
*
*
*    select WERKS,email
*    from ZLEST0225
*    FOR ALL ENTRIES IN @it_parc
*      where werks = @IT_parc-werks
*            into TABLE @send_filial.
*
*
*
*  ENDFORM.
