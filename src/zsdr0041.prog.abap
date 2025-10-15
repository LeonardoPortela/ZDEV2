*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 13/06/2014                                              &*
*& Descrição: Informativo notas de compra com fins especifico de      &*
*&            exportação                                              &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  zsdr0041.

TYPE-POOLS: zmemo.
TYPES: BEGIN OF ty_bnfdoc,
         docnum TYPE j_1bnfdoc-docnum,
         land1  TYPE j_1bnfdoc-land1,
         regio  TYPE j_1bnfdoc-regio,
       END OF ty_bnfdoc,

       BEGIN OF ty_bnflin,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         matnr  TYPE j_1bnflin-matnr,
       END OF ty_bnflin,

       BEGIN OF ty_prazo,
         land1 TYPE j_1bnfdoc-land1,
         regio TYPE j_1bnfdoc-regio,
         matnr TYPE j_1bnflin-matnr,
       END OF ty_prazo,

       BEGIN OF ty_kna1_aux,
         kunnr TYPE kna1-kunnr,
         werks TYPE zmemo_export_acomp-werks,
       END OF ty_kna1_aux.

CONSTANTS: c_x TYPE c VALUE 'X'.

DATA: it_exportacoes  TYPE TABLE OF zexport_acomp INITIAL SIZE 0 WITH HEADER LINE,
      it_export_acomp TYPE TABLE OF zmemo_export_acomp INITIAL SIZE 0 WITH HEADER LINE,
      it_bnfdoc       TYPE TABLE OF ty_bnfdoc WITH HEADER LINE,
      it_0033         TYPE TABLE OF zsdt0033 WITH HEADER LINE,
      it_lfa1         TYPE TABLE OF lfa1 WITH HEADER LINE,
      it_kna1         TYPE TABLE OF kna1 WITH HEADER LINE,
      it_kna1_aux     TYPE TABLE OF ty_kna1_aux WITH HEADER LINE,
      it_adr6         TYPE TABLE OF adr6 WITH HEADER LINE,
      it_0088         TYPE TABLE OF zsdt0088 WITH HEADER LINE,
      it_prazo        TYPE TABLE OF ty_prazo WITH HEADER LINE,
      it_notas        LIKE TABLE OF zexport_notas,
      it_notas3       LIKE TABLE OF zexport_notas,
      wa_exportacoes  TYPE zexport_acomp,
      wa_notas        LIKE zexport_notas,
      wa_export_acomp TYPE zmemo_export_acomp,
      vg_quantidade   TYPE j_1bnetqty,
      vg_quantcompe   TYPE j_1bnetqty,
      vg_quantacomp   TYPE j_1bnetqty.


PARAMETERS in_emp TYPE j_1bnfdoc-bukrs  NO-DISPLAY.
PARAMETERS in_bra TYPE j_1bnfdoc-branch NO-DISPLAY.

START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM organiza_dados.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .
  DATA: wl_index   TYPE sy-tabix,
        wl_setleaf TYPE setleaf,
        tl_perioe  TYPE lxhme_range_date_t,
        wl_perioe  TYPE lxhme_range_date,

        dt_inicial TYPE sy-datum,
        it_nf      TYPE zmemo_nf_t,
        t_cfop     TYPE TABLE OF zmemo_cfop.

  CLEAR: it_exportacoes[], it_notas[], it_export_acomp[], wl_perioe.
  REFRESH: tl_perioe.

*  SELECT SINGLE *
*    FROM SETLEAF
*    INTO WL_SETLEAF
*     WHERE SETNAME EQ 'MAGGI_ZSDR0041_DATA_INI'.

*  IF SY-SUBRC IS INITIAL.
*    WL_PERIOE-LOW = WL_SETLEAF-VALFROM.
*    WL_PERIOE-SIGN    = 'I'.
*    WL_PERIOE-OPTION  = 'GE'.
*
*    APPEND WL_PERIOE TO TL_PERIOE.
*  ENDIF.

  tl_perioe = VALUE #(
                      ( sign    = 'I' option  = 'GE' low = sy-datum - 240 )
                     ).

  dt_inicial = sy-datum - 240.

  vg_quantidade = 0.
  vg_quantcompe = 0.
  vg_quantacomp = 0.

  SELECT dc~docnum
    INTO TABLE @DATA(it_doc)
    FROM j_1bnfdoc AS dc
   WHERE dc~partyp EQ 'V'
     AND dc~direct EQ '1'
     AND dc~cancel EQ ' '
     AND dc~doctyp NE '5'
     AND dc~docdat >= @dt_inicial
     AND dc~bukrs  EQ @in_emp
     AND dc~branch EQ @in_bra.

  IF it_doc IS NOT INITIAL.

    CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
      TABLES
        cfops = t_cfop.

    SELECT li~docnum li~itmnum li~matnr li~reftyp li~refkey li~refitm
        FROM j_1bnflin AS li
      INTO CORRESPONDING FIELDS OF TABLE it_nf
       FOR ALL ENTRIES IN it_doc
         WHERE li~docnum EQ it_doc-docnum
           AND li~cfop   IN t_cfop.

  ENDIF.

  CHECK it_nf IS NOT INITIAL.

  CALL FUNCTION 'Z_EXPORT_TERCEIRO_ACOMP'
    EXPORTING
      t_period         = tl_perioe[]
      exportadores     = c_x
      notas_exportacao = c_x
      p_direcao        = '1'
      t_nf             = it_nf
    TABLES
      it_exportacoes   = it_exportacoes
      it_notas         = it_notas
    EXCEPTIONS
      cfops_saida      = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Início - Sara Oikawa - CS2019001322 - 27.05.2020
* O Sistema deverá verificar se a nota está ou não vinculada (total ou parcial) a um processo de exportação
* Só deverá considerar, para envio de e-mail, as notas que não tenham nenhuma vinculação
  LOOP AT it_notas INTO wa_notas.
    IF wa_notas-quantidade NE wa_notas-quantaplan.
      DELETE it_notas INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
* Fim - Sara Oikawa - CS2019001322 - 27.05.2020


  SORT  it_notas BY dt_emissao ASCENDING quantaplan.
  it_notas3[] = it_notas[].

  DELETE it_notas3 WHERE quantaplan LE 0.
  DELETE it_exportacoes WHERE quantaplan LE 0.

  LOOP AT it_exportacoes INTO wa_exportacoes.
    vg_quantidade = vg_quantidade + wa_exportacoes-quantidade.
    vg_quantcompe = vg_quantcompe + wa_exportacoes-quantcompe.
    vg_quantacomp = vg_quantacomp + wa_exportacoes-quantacomp.

    CLEAR wa_export_acomp-dt_emissao.
    "Pega a data da primeira nota com  saldo
    LOOP AT it_notas3 INTO wa_notas WHERE bukrs     = wa_exportacoes-bukrs
                                     AND werks      = wa_exportacoes-werks
                                     AND exportador = wa_exportacoes-exportador
                                     AND produto    = wa_exportacoes-produto
                                     AND unidade    = wa_exportacoes-unidade.
      IF wa_notas-quantaplan GT 0.
        wa_export_acomp-dt_emissao = wa_notas-dt_emissao.
        EXIT.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING wa_exportacoes TO wa_export_acomp.
    APPEND wa_export_acomp TO it_export_acomp.
  ENDLOOP.

  DELETE it_export_acomp WHERE quantidade = 1.
*  DELETE IT_NOTAS        WHERE QUANTIDADE = 1
*                            OR SALDO LE 0.
  DELETE it_notas        WHERE quantidade = 1
                            OR quantaplan LE 0.

  REFRESH it_notas3.

  CHECK it_export_acomp[] IS NOT INITIAL.

  IF it_notas[] IS NOT INITIAL.
    REFRESH: it_kna1_aux.

    LOOP AT it_export_acomp INTO wa_export_acomp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_export_acomp-werks
        IMPORTING
          output = it_kna1_aux-kunnr.

      it_kna1_aux-werks = wa_export_acomp-werks.

      APPEND it_kna1_aux.
      CLEAR it_kna1_aux.

    ENDLOOP.

    IF it_kna1_aux[] IS NOT INITIAL.
      SELECT *
        FROM kna1
        INTO TABLE it_kna1
         FOR ALL ENTRIES IN it_kna1_aux
          WHERE kunnr EQ it_kna1_aux-kunnr.

    ENDIF.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
       FOR ALL ENTRIES IN it_export_acomp
       WHERE lifnr EQ it_export_acomp-exportador.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM adr6
        INTO TABLE it_adr6
         FOR ALL ENTRIES IN it_lfa1
         WHERE addrnumber EQ it_lfa1-adrnr.
    ENDIF.

**  Busca notas q o email ja foi enviado
    SELECT *
      FROM zsdt0088
      INTO TABLE it_0088
       FOR ALL ENTRIES IN it_notas
       WHERE docnum EQ it_notas-docnum
         AND itmnum EQ it_notas-itmnum.

** Elimina notas q os emails ja foram enviados
    SORT it_0088 BY docnum itmnum.

* Início - Sara Oikawa - CS2019001322 - 27.05.2020
*    LOOP AT IT_NOTAS INTO WA_NOTAS.
*      WL_INDEX = SY-TABIX.
*
*      READ TABLE IT_0088
*        WITH KEY DOCNUM = WA_NOTAS-DOCNUM
*                 ITMNUM = WA_NOTAS-ITMNUM
*                  BINARY SEARCH.
*      IF SY-SUBRC IS INITIAL.
*        DELETE IT_NOTAS INDEX WL_INDEX.
*      ENDIF.
*    ENDLOOP.
* Fim - Sara Oikawa - CS2019001322 - 27.05.2020

*    IF IT_NOTAS[] IS NOT INITIAL.
*      SELECT DOCNUM LAND1 REGIO
*        FROM J_1BNFDOC
*        INTO TABLE IT_BNFDOC
*         FOR ALL ENTRIES IN IT_NOTAS
*         WHERE DOCNUM EQ IT_NOTAS-DOCNUM.

*    IF SY-SUBRC IS INITIAL.
*      SELECT DOCNUM ITMNUM MATNR
*        FROM J_1BNFlin
*        INTO TABLE IT_BNFlin
*         FOR ALL ENTRIES IN IT_NOTAS
*         WHERE DOCNUM EQ IT_NOTAS-DOCNUM
*           and ITMNUM eq IT_NOTAS-ITMNUM.
*    ENDIF.
    SORT it_lfa1 BY lifnr.

    LOOP AT it_notas INTO wa_notas.
      READ TABLE it_lfa1
        WITH KEY lifnr = wa_notas-exportador
                  BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_prazo TRANSPORTING NO FIELDS
          WITH KEY land1 = it_lfa1-land1
                   regio = it_lfa1-regio
                   matnr = wa_notas-produto.

        IF sy-subrc IS NOT INITIAL.
          MOVE : it_lfa1-land1  TO it_prazo-land1,
                 it_lfa1-regio  TO it_prazo-regio,
                 wa_notas-produto TO it_prazo-matnr.

          APPEND it_prazo.
          CLEAR: it_prazo.
        ENDIF.
      ENDIF.
    ENDLOOP.

** Busca prazo para envio do email
    IF it_prazo[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0033
        INTO TABLE it_0033
         FOR ALL ENTRIES IN it_prazo
         WHERE land1 EQ it_prazo-land1
           AND regio EQ it_prazo-regio
           AND matnr EQ it_prazo-matnr.

    ENDIF.
*    ENDIF.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_dados .
  DATA: tl_notas      TYPE TABLE OF zsds014 WITH HEADER LINE,
        tl_input_0088 TYPE TABLE OF zsdt0088 WITH HEADER LINE,
        wl_dias       TYPE zsdt0033-alerta,
        wl_subrc      TYPE sy-subrc,
        wl_index      TYPE sy-index.

  SORT: "IT_BNFDOC BY DOCNUM,
        it_0033     BY land1 regio matnr,
        it_lfa1     BY lifnr,
        it_adr6     BY addrnumber,
        it_kna1_aux BY werks,
        it_kna1     BY kunnr.


  LOOP AT it_export_acomp INTO wa_export_acomp.
    CLEAR: wl_subrc, it_lfa1, it_adr6.
    REFRESH: tl_notas.
    READ TABLE it_lfa1
      WITH KEY lifnr = wa_export_acomp-exportador
               BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      READ TABLE it_adr6
            WITH KEY addrnumber = it_lfa1-adrnr
                     BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT it_notas INTO wa_notas WHERE bukrs      = wa_export_acomp-bukrs
                                         AND werks      = wa_export_acomp-werks
                                         AND exportador = wa_export_acomp-exportador
                                         AND produto    = wa_export_acomp-produto
                                         AND unidade    = wa_export_acomp-unidade.

*          READ TABLE IT_BNFDOC
*            WITH KEY DOCNUM = WA_NOTAS-DOCNUM
*                      BINARY SEARCH.
*          IF SY-SUBRC IS INITIAL.
          wl_index = sy-tabix.
          wl_dias = sy-datum - wa_notas-dt_emissao.

          READ TABLE it_0033
            WITH KEY land1 = it_lfa1-land1
                     regio = it_lfa1-regio
                     matnr = wa_notas-produto
                     BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL
          OR it_0033-alerta LT wl_dias.

            MOVE: wa_notas-nfenum     TO tl_notas-nfenum,
                  wa_notas-docnum     TO tl_notas-docnum,
                  wa_notas-itmnum     TO tl_notas-itmnum,
                  wa_notas-dt_emissao TO tl_notas-docdat,
                  wa_notas-quantidade TO tl_notas-menge.

            APPEND tl_notas.
            CLEAR: tl_notas.
            DELETE it_notas INDEX wl_index .
          ENDIF.
*          ENDIF.
        ENDLOOP.
        IF tl_notas[] IS NOT INITIAL.
          READ TABLE it_kna1_aux
            WITH KEY werks = wa_export_acomp-werks
                     BINARY SEARCH.

          READ TABLE it_kna1
            WITH KEY kunnr = it_kna1_aux-kunnr
                     BINARY SEARCH.

          SORT tl_notas BY docnum itmnum.

          PERFORM envia_email TABLES   tl_notas
                              USING    wa_export_acomp
                                       it_adr6-smtp_addr
                                       it_kna1-stcd1
                              CHANGING wl_subrc.

          IF wl_subrc IS INITIAL.
            REFRESH: tl_input_0088.
            LOOP AT tl_notas.
              MOVE: tl_notas-docnum         TO tl_input_0088-docnum,
                    tl_notas-itmnum         TO tl_input_0088-itmnum,
                    wa_export_acomp-bukrs   TO tl_input_0088-bukrs,
                    wa_export_acomp-werks   TO tl_input_0088-branch,
                    tl_notas-docdat         TO tl_input_0088-docdat,
                    sy-mandt                TO tl_input_0088-mandt.

              APPEND tl_input_0088.
              CLEAR: tl_input_0088.

            ENDLOOP.

            IF tl_input_0088[] IS NOT INITIAL.
              MODIFY zsdt0088 FROM TABLE tl_input_0088.
              COMMIT WORK.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL_LIBERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email TABLES   tl_notas  STRUCTURE zsds014
                 USING    wl_header TYPE      zmemo_export_acomp
                          wl_dest
                          wl_cnpj_empresa TYPE kna1-stcd1
                 CHANGING wl_subrc  TYPE      sy-subrc.

  DATA: ls_control        TYPE ssfctrlop,
        ls_options        TYPE ssfcompop,
        xsfparam_line     TYPE ssfxsfp,
        job_output_info   TYPE ssfcrescl,
        v_bin_filesize    TYPE i,
        it_docs           TYPE STANDARD TABLE OF docs,
        it_lines          TYPE STANDARD TABLE OF tline,
        tl_body           TYPE TABLE OF tline WITH HEADER LINE,
        lv_fname          TYPE rs38l_fnam,
        lv_mail_recipient TYPE swotobjid,
        lv_mail_sender    TYPE swotobjid,
        lv_control        TYPE ssfctrlop,
        lv_name           TYPE so_name,
        lv_output         TYPE ssfcompop,
        wl_zmeng(20),
        wl_dmbtr(20),
        wl_vlrtot(20),
        tl_html           TYPE trfresult,
        tl_graphics       TYPE tsf_xsf_gr.

  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
*       Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
*       Work Area declarations
        wa_objhead  TYPE soli_tab,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
*       Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i,
        vl_form     TYPE tdsfname,
        vl_name     TYPE rs38l_fnam.

  MOVE 8 TO wl_subrc.
  vl_form = 'ZSDS0006'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*  Impresora
  ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = 'X'.

  CLEAR:job_output_info.
  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
      i_parid            = wl_header-exportador
      i_matnr            = wl_header-produto
      i_regio            = wl_header-regiao
      i_name1            = wl_header-exportadorn
      i_maktx            = wl_header-produton
      i_documento        = wl_header-exportcnpj
      i_empresa          = wl_header-werksn
      i_cnpj_empresa     = wl_cnpj_empresa
    IMPORTING
      job_output_info    = job_output_info
    TABLES
      ti_notas           = tl_notas
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ELSE.

*--- Convert OTF to PDF
*      CALL FUNCTION 'CONVERT_OTF_2_PDF'
*        IMPORTING
*          BIN_FILESIZE           = V_BIN_FILESIZE
*        TABLES
*          OTF                    = JOB_OUTPUT_INFO-OTFDATA
*          DOCTAB_ARCHIVE         = IT_DOCS
*          LINES                  = IT_LINES
*        EXCEPTIONS
*          ERR_CONV_NOT_POSSIBLE  = 1
*          ERR_OTF_MC_NOENDMARKER = 2
*          OTHERS                 = 3.
*      IF SY-SUBRC <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.

*      IF SY-UNAME NE 'ABAP'.
    i_otf[] = job_output_info-otfdata[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
      IMPORTING
        bin_filesize          = v_bin_filesize
      TABLES
        otf                   = i_otf
        lines                 = i_tline
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.
    IF sy-subrc EQ 0.
    ENDIF.
    LOOP AT i_tline.
      TRANSLATE i_tline USING '~'.
      CONCATENATE wa_buffer i_tline INTO wa_buffer.
    ENDLOOP.
    TRANSLATE wa_buffer USING '~'.
    DO.
      i_record = wa_buffer.
      APPEND i_record.
      SHIFT wa_buffer LEFT BY 255 PLACES.
      IF wa_buffer IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
* Attachment
    REFRESH: i_reclist,
             i_objtxt,
             i_objbin,
             i_objpack.

    CLEAR wa_objhead.
    i_objbin[] = i_record[].

* Create Message Body Title and Description
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'ST'
        language                = sy-langu
        name                    = 'Z_MEMO_EMAIL'
        object                  = 'TEXT'
      TABLES
        lines                   = tl_body
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    LOOP AT tl_body.
      i_objtxt = tl_body-tdline.
      APPEND i_objtxt.
    ENDLOOP.

    DESCRIBE TABLE i_objtxt LINES v_lines_txt.
    READ TABLE i_objtxt INDEX v_lines_txt.
    wa_doc_chng-obj_name = 'smartform'.
    wa_doc_chng-expiry_dat = sy-datum + 10.
    wa_doc_chng-obj_descr = |{ sy-sysid } { in_emp }/{ in_bra } Prorrogação do Prazo para Comprovação de Exportação|.
*        CONCATENATE 'Solicitação de Venda  Nº' WG_HEADER-NRO_SOL_OV INTO WA_DOC_CHNG-OBJ_DESCR
*          SEPARATED BY SPACE.
*     = 'smartform'.
    wa_doc_chng-sensitivty = 'F'.
    wa_doc_chng-doc_size = v_lines_txt * 255.
* Main Text
    CLEAR i_objpack-transf_bin.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    i_objpack-body_num = v_lines_txt.
    i_objpack-doc_type = 'RAW'.
    APPEND i_objpack.
* Attachment (pdf-Attachment)
    i_objpack-transf_bin = 'X'.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    DESCRIBE TABLE i_objbin LINES v_lines_bin.
    READ TABLE i_objbin INDEX v_lines_bin.
    i_objpack-doc_size = v_lines_bin * 255 .
    i_objpack-body_num = v_lines_bin.
    i_objpack-doc_type = 'PDF'.
    i_objpack-obj_name = 'smart'.
    i_objpack-obj_descr = wl_header-exportador.
    APPEND i_objpack.
*        LOOP AT TL_ZMAIL.
    CLEAR i_reclist.

*    i_reclist-receiver = wl_dest.
*    i_reclist-rec_type = 'U'.
*    APPEND i_reclist.

    i_reclist-receiver = 'exec.comprovacao@amaggi.com.br'.
    i_reclist-rec_type = 'U'.
*    i_reclist-copy = 'X'.
    APPEND i_reclist.
*        ENDLOOP.
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = wa_doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = i_objpack
        object_header              = wa_objhead
        contents_bin               = i_objbin
        contents_txt               = i_objtxt
        receivers                  = i_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc NE 0.
*WRITE:/ ‘Error When Sending the File’, SY-SUBRC.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ocorreu um erro ao enviar o e-mail'.
      MOVE sy-subrc TO wl_subrc.
    ELSE.
      MESSAGE s836(sd) WITH 'E-mail enviado com sucesso'.
      MOVE sy-subrc TO wl_subrc.
*WRITE:/ ‘Mail sent’.
    ENDIF.

*      ENDIF.
  ENDIF.
*  ENDIF.

ENDFORM.                    " ENVIA_EMAIL_LIBERACAO
