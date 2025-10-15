*&---------------------------------------------------------------------*
*& Report  Z_MM_NFISCAL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_mm_nfiscal.

*"----------------------------------------------------------------------
*"*"Interface local:
DATA: t_bdydoc  TYPE TABLE OF zmme_nf_sigam    WITH HEADER LINE INITIAL SIZE 0,
      t_bdylin  TYPE TABLE OF zmme_nflin_sigam WITH HEADER LINE INITIAL SIZE 0,
      t_bdystx  TYPE TABLE OF zmme_nftax_sigam WITH HEADER LINE INITIAL SIZE 0,
      t_partner TYPE TABLE OF zmme_nfparc_siga WITH HEADER LINE INITIAL SIZE 0.

TABLES: j_1baa, j_1batl1, j_1batl2, j_1batl4a, j_1batl5.

TYPES: BEGIN OF ty_nf,
         nfnum   TYPE j_1bnfdoc-nfnum,
         docnum  TYPE j_1bnfdoc-docnum,
         obj_key TYPE znfeinfo-obj_key,
       END   OF  ty_nf.


DATA: w_header     LIKE bapi_j_1bnfdoc,
      w_nfcheck    LIKE bapi_j_1bnfcheck,
      t_nf         TYPE TABLE OF ty_nf,
      wa_nf        TYPE ty_nf,


      v_docnum     LIKE bapi_j_1bnfdoc-docnum,
*        ti_partner    like bapi_j_1bnfnad occurs 0 with header line,
      t_item       LIKE bapi_j_1bnflin OCCURS 0 WITH HEADER LINE,
      t_item_add   LIKE bapi_j_1bnflin_add OCCURS 0 WITH HEADER LINE,
      t_item_tax   LIKE bapi_j_1bnfstx OCCURS 0 WITH HEADER LINE,
      t_msg        LIKE bapi_j_1bnfftx OCCURS 0 WITH HEADER LINE,
      t_refmsg     LIKE bapi_j_1bnfref OCCURS 0 WITH HEADER LINE,
      t_otpart     LIKE bapi_j_1bnfcpd OCCURS 0 WITH HEADER LINE,
      t_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
      v_kunnr      LIKE kna1-kunnr,
      v_lifnr      LIKE kna1-lifnr,
      v_matnr      LIKE mara-matnr,
      v_docstat    LIKE bapi_j_1bnfdoc-docstat,
      v_text01(72),
      v_text02(72),
      v_text03(72),
      v_text04(72),
      v_text05(72),
      v_text06(72),
      v_text07(72),
      v_text08(72),
      v_text09(72),
      v_text10(72).

DATA: vl_campo(20),
      vl_index(02) TYPE n,
      vl_docnum    LIKE j_1bnfdoc-docnum,
      vg_nfenum    TYPE j_1bnfnum9.

FIELD-SYMBOLS: <fs_text> TYPE any.

DATA: oo_exceptions TYPE REF TO cx_root.

DATA: w_partner_record  LIKE j_1binnad,
      w_material_record LIKE mara,
      w_makt_text       LIKE makt,
      w_mbew            LIKE mbew.

DATA: t_j1bnfnad LIKE bapi_j_1bnfnad OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF t_log OCCURS 0.
        INCLUDE STRUCTURE zfie_ret_document.
DATA: END OF t_log.

DATA: BEGIN OF t_log2 OCCURS 0.
        INCLUDE STRUCTURE zfie_ret_document.
DATA: END OF t_log2.

DATA: BEGIN OF wa_nfe.
        INCLUDE STRUCTURE znfeinfo.
DATA: END OF wa_nfe.

DATA:  t_nfe   LIKE STANDARD TABLE OF wa_nfe.

CLEAR t_nfe.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
DATA: vg_job TYPE i.

SELECT SINGLE COUNT(*) INTO vg_job
  FROM tbtco
 WHERE jobname EQ 'NFISCAL_INTERFACE_SIGAM'
   AND status EQ 'R'.

IF ( vg_job EQ 1 ).

  PERFORM : seleciona_dados,
            processa_nf.

ENDIF.

*&---------------------------------------------------------------------*
*&      Form  seleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_dados.

  REFRESH : t_bdydoc, t_bdylin, t_bdystx,  t_partner.

  SELECT *
    INTO TABLE t_bdydoc
    FROM zmme_nf_sigam
   WHERE zrg_atualizado = 'N'.

  IF sy-subrc IS INITIAL.

    SELECT *
      FROM zmme_nflin_sigam
      INTO TABLE t_bdylin
       FOR ALL ENTRIES IN t_bdydoc
     WHERE obj_key = t_bdydoc-obj_key.

    SELECT *
      FROM zmme_nftax_sigam
      INTO TABLE t_bdystx
       FOR ALL ENTRIES IN t_bdydoc
     WHERE obj_key = t_bdydoc-obj_key.

    SELECT *
      FROM zmme_nfparc_siga
      INTO TABLE t_partner
       FOR ALL ENTRIES IN t_bdydoc
     WHERE obj_key = t_bdydoc-obj_key.
  ENDIF.

ENDFORM.                    "SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  processa_nf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM processa_nf.

  PERFORM processa_reg_lido.

  LOOP AT t_bdydoc.

    REFRESH: t_item, t_item_add, t_item_tax, t_j1bnfnad,
             t_msg, t_refmsg, t_otpart, t_bapiret2.

    CLEAR: w_header, w_nfcheck, vl_docnum, v_docnum.

    IF t_bdydoc-nfe IS INITIAL.
* Validação de notas
      SELECT SINGLE docnum
        FROM j_1bnfdoc
        INTO (vl_docnum)
       WHERE nftype EQ t_bdydoc-nftype
         AND docdat EQ t_bdydoc-docdat
         AND pstdat EQ t_bdydoc-pstdat
         AND nfnum  EQ t_bdydoc-nfnum
         AND bukrs  EQ t_bdydoc-bukrs
         AND branch EQ t_bdydoc-branch
         AND parid  EQ t_bdydoc-parid
         AND cancel NE 'X'.
    ELSE.
* Validação de notas
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = t_bdydoc-nfnum
        IMPORTING
          output = vg_nfenum.

      SELECT SINGLE docnum
        FROM j_1bnfdoc
        INTO (vl_docnum)
       WHERE nftype EQ t_bdydoc-nftype
         AND docdat EQ t_bdydoc-docdat
         AND pstdat EQ t_bdydoc-pstdat
         AND bukrs  EQ t_bdydoc-bukrs
         AND branch EQ t_bdydoc-branch
         AND parid  EQ t_bdydoc-parid
         AND nfe    EQ 'X'
         AND nfenum EQ vg_nfenum
         AND cancel NE 'X'.
    ENDIF.

    IF ( sy-subrc EQ 0 ).
      CLEAR t_bapiret2.
      t_bapiret2-message_v1 = t_bdydoc-nfnum.
      t_bapiret2-message_v2 = vl_docnum.
      v_docnum = vl_docnum.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          id         = 'Z01'
          number     = '009'
          language   = sy-langu
          textformat = 'ASC'
          message_v1 = t_bapiret2-message_v1
          message_v2 = t_bapiret2-message_v2
        IMPORTING
          message    = t_bapiret2-message.

      t_bapiret2-id         = 'Z01'.
      t_bapiret2-number     = '009'.
      t_bapiret2-type       = 'S'.
      t_bapiret2-message_v1 = vl_docnum.
      "T_BAPIRET2-MESSAGE_V2 = T_BDYDOC-NFNUM.

      APPEND t_bapiret2.

    ELSE.

*   Cabeçalho

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = t_bdydoc-parid
        IMPORTING
          output = t_bdydoc-parid.

      MOVE-CORRESPONDING t_bdydoc TO w_header.
      v_docstat = w_header-docstat.
      CLEAR w_header-docstat.

      w_header-manual = 'X'.

*   Form
      SELECT SINGLE * FROM j_1baa
         WHERE nftype = t_bdydoc-nftype.

      IF sy-subrc = 0.
        w_header-doctyp = j_1baa-doctyp.
        w_header-direct = j_1baa-direct.
        w_header-form   = j_1baa-form.
        w_header-model  = j_1baa-model.
        w_header-entrad = j_1baa-entrad.
*        w_header-parvw  = j_1baa-parvw.
      ENDIF.

*      IF NOT t_bdydoc-nfe IS INITIAL.
*        w_header-form = 'SG55'.
*      ENDIF.

*   Buscar dados dos parceiros
      REFRESH t_j1bnfnad.
      LOOP AT t_partner WHERE obj_key = t_bdydoc-obj_key.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = t_partner-parid
          IMPORTING
            output = t_partner-parid.

        t_j1bnfnad-parid = t_partner-parid.
        t_j1bnfnad-parvw = t_partner-parvw.
        APPEND t_j1bnfnad.

      ENDLOOP.


      LOOP AT t_bdylin WHERE obj_key = t_bdydoc-obj_key.

        CLEAR: t_item, t_item_add.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = t_bdylin-matnr
          IMPORTING
            output       = t_bdylin-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = t_bdylin-itmtyp
          IMPORTING
            output = t_bdylin-itmtyp.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = t_bdylin-taxlw1
          IMPORTING
            output = t_bdylin-taxlw1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = t_bdylin-taxlw2
          IMPORTING
            output = t_bdylin-taxlw2.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = t_bdylin-taxlw4
          IMPORTING
            output = t_bdylin-taxlw4.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = t_bdylin-taxlw5
          IMPORTING
            output = t_bdylin-taxlw5.

        MOVE-CORRESPONDING t_bdylin TO t_item. "#EC CI_FLDEXT_OK[2215424]
        CLEAR t_item-cfop.
        v_matnr = t_bdylin-matnr.
        t_item-bwkey = t_item-werks.
        t_item-netwr = t_item-netpr * t_item-menge.

*        t_item-meins = 'CDA'.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input    = t_item-meins
            language = 'P'
          IMPORTING
            output   = t_item-meins.

        MOVE t_bdylin-cfop TO t_item-cfop_10.

        CALL FUNCTION 'Z_VALID_CFOBR_INPUT'
          EXPORTING
            input            = t_item-cfop_10
          IMPORTING
            output           = t_item-cfop_10
          EXCEPTIONS
            enter_valid_cfop = 1
            OTHERS           = 2.

        IF ( sy-subrc NE 0 ).
          CLEAR t_bapiret2.
          t_bapiret2-message_v1 = t_item-cfop_10.

          CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
            EXPORTING
              id         = '8B'
              number     = '036'
              language   = sy-langu
              textformat = 'ASC'
              message_v1 = t_bapiret2-message_v1
            IMPORTING
              message    = t_bapiret2-message.

          t_bapiret2-id         = '8B'.
          t_bapiret2-number     = '036'.
          t_bapiret2-type       = 'E'.
          t_bapiret2-message_v1 = t_item-cfop_10.

          APPEND t_bapiret2.
          EXIT.

        ENDIF.

        CALL FUNCTION 'J_1B_MATERIAL_READ'
          EXPORTING
            matnr                = v_matnr
            val_area             = t_item-bwkey
            val_type             = t_item-bwtar
            i_werks              = t_item-werks
            language             = 'P'
          IMPORTING
            nbm                  = t_item-nbm
            indus3               = t_item-indus3
            matuse               = t_item-matuse
            matorg               = t_item-matorg
            ownpro               = t_item-ownpro
            material_record      = w_material_record
            material_text_record = w_makt_text
            valuation_record     = w_mbew
          EXCEPTIONS
            material_not_found   = 1
            valuation_not_found  = 2
            OTHERS               = 3.

        IF ( sy-subrc NE 0 ).
          CLEAR t_bapiret2.
          t_bapiret2-message_v1 = sy-msgv1.
          t_bapiret2-message_v2 = sy-msgv2.
          t_bapiret2-message_v3 = sy-msgv3.
          t_bapiret2-message_v4 = sy-msgv4.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO t_bapiret2-message.
          t_bapiret2-id         = sy-msgid.
          t_bapiret2-number     = sy-msgno.
          t_bapiret2-type       = sy-msgty.
          APPEND t_bapiret2.
          EXIT.
        ENDIF.

        SELECT * FROM j_1batl1
           WHERE taxlaw = t_item-taxlw1.
        ENDSELECT.

        IF sy-subrc = 0.
          t_item-taxsit = j_1batl1-taxsit.
        ENDIF.

        SELECT * FROM j_1batl2
           WHERE taxlaw = t_item-taxlw2.
        ENDSELECT.

        IF sy-subrc = 0.
          t_item-taxsi2 = j_1batl2-taxsit.
        ENDIF.

        SELECT * FROM j_1batl4a
          WHERE taxlaw = t_item-taxlw4.
        ENDSELECT.

        IF sy-subrc = 0.
          t_item-taxsi4 = j_1batl4a-taxsit.
        ENDIF.

        SELECT * FROM j_1batl5
          WHERE taxlaw = t_item-taxlw5.
        ENDSELECT.

        IF sy-subrc = 0.
          t_item-taxsi5 = j_1batl5-taxsit.
        ENDIF.

        IF t_bdydoc-nftype EQ 'Z9'.
          t_item-tmiss = 'X'.
        ELSE.
          CLEAR t_item-tmiss.
        ENDIF.

        t_item-matkl = w_material_record-matkl.
        t_item-maktx = w_makt_text-maktx.
        t_item-incltx = 'X'.
        APPEND t_item.


        w_header-waerk = 'BRL'.

        MOVE-CORRESPONDING t_item TO t_item_add.
        APPEND t_item_add.

      ENDLOOP.

*   Impostos

      IF ( t_bapiret2[] IS INITIAL ).
        LOOP AT t_bdystx WHERE obj_key = t_bdydoc-obj_key.
          CLEAR t_item_tax.
          MOVE-CORRESPONDING t_bdystx TO t_item_tax.
          APPEND t_item_tax.
        ENDLOOP.

*   Mensagens
        CLEAR: v_text01, v_text02, v_text03, v_text04, v_text05, v_text06,
               v_text07, v_text08, v_text09, v_text10.

        SPLIT t_bdydoc-text AT '/N' INTO v_text01 v_text02 v_text03
           v_text04 v_text05 v_text06 v_text07 v_text08 v_text09 v_text10.

*  DOCNUM
        DO.
          vl_index = sy-index.
          CONCATENATE 'v_text' vl_index INTO vl_campo.
          ASSIGN (vl_campo) TO <fs_text>.

          IF ( <fs_text> IS INITIAL ) OR ( sy-index GT 10 ).
            EXIT.
          ELSE.
            t_msg-seqnum  = 1.
            t_msg-linnum  = sy-index.
            t_msg-manual  = 'X'.
            t_msg-message = <fs_text>.
            APPEND t_msg.
          ENDIF.

        ENDDO.


        SORT t_msg BY docnum seqnum linnum.

*   Efetua a criação do docto fiscal
        TRY.

            SORT t_item BY docnum itmnum.
            SORT t_item_add BY docnum itmnum.
            SORT t_item_tax BY docnum itmnum.

            IF ( w_header-form IS INITIAL ) AND ( NOT w_header-nfe IS INITIAL ).
              w_nfcheck-chekcon = 'X'.
            ENDIF.

            w_header-access_key = t_bdydoc-nu_chave.
            IF ( w_header-doctyp EQ '1' ) OR
               ( w_header-doctyp EQ '4' AND w_header-form EQ 'NF57').
              CLEAR w_header-docstat.
            ELSE.
              w_header-docstat    = t_bdydoc-docstat.
            ENDIF.

            IF t_bdydoc-xmlvers = '2.00'.
              w_header-docnum9    = t_bdydoc-docnum9+1(8).
              IF w_header-form EQ 'NF55' OR w_header-form EQ 'NF56' .
                CLEAR w_header-docstat.
              ENDIF.
            ELSE.
              w_header-docnum9    = t_bdydoc-docnum9.
            ENDIF.

            w_header-tpemis     = t_bdydoc-docnum9(1).
            w_header-manual     = 'X'.

            CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                obj_header     = w_header
                nfcheck        = w_nfcheck
              IMPORTING
                e_docnum       = v_docnum
              TABLES
                obj_partner    = t_j1bnfnad
                obj_item       = t_item
                obj_item_add   = t_item_add
                obj_item_tax   = t_item_tax
                obj_header_msg = t_msg
                obj_refer_msg  = t_refmsg
                obj_ot_partner = t_otpart
                return         = t_bapiret2.

          CLEANUP INTO oo_exceptions.
            CLEAR t_bapiret2.
            t_bapiret2-type    = 'A'.
            t_bapiret2-id      = 'Z01'.
            t_bapiret2-number  = '001'.
            t_bapiret2-message = oo_exceptions->get_text( ).
            APPEND t_bapiret2.
        ENDTRY.
      ENDIF.
    ENDIF.

* Log
    t_bdydoc-dt_atualizacao = sy-datum.
    t_bdydoc-hr_atualizacao = sy-uzeit.

    CLEAR t_log.
    t_log-obj_key        = t_bdydoc-obj_key.
    t_log-interface      = t_bdydoc-interface.
    t_log-dt_atualizacao = t_bdydoc-dt_atualizacao.
    t_log-hr_atualizacao = t_bdydoc-hr_atualizacao.

* Se não gerou documento, efetua rollback
    IF v_docnum IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      LOOP AT t_bapiret2 WHERE type = 'E' OR type = 'A' OR type = 'S'.
        t_log-type       = t_bapiret2-type.
        t_log-id         = t_bapiret2-id.
        t_log-num        = t_bapiret2-number.
        t_log-message    = t_bapiret2-message.
        t_log-message_v1 = t_bapiret2-message_v1.
        t_log-message_v2 = t_bapiret2-message_v2.
        t_log-message_v3 = t_bapiret2-message_v3.
        t_log-message_v4 = t_bapiret2-message_v4.
        APPEND t_log.
      ENDLOOP.

    ELSE.
* Se gerou documento, efetua commit para gravar

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF NOT t_bdydoc-nfe IS INITIAL.
        CLEAR wa_nfe.
        wa_nfe-obj_key = t_bdydoc-obj_key.
        wa_nfe-docnum  = v_docnum.
        wa_nfe-nfenum  = t_bdydoc-nfenum.
        wa_nfe-docstat = v_docstat.
        wa_nfe-docsta  = v_docstat.
        wa_nfe-code    = t_bdydoc-code.
        wa_nfe-nfnum9  = t_bdydoc-nfenum.
        wa_nfe-cdv     = t_bdydoc-cdv.
        wa_nfe-scssta  = '2'.
        wa_nfe-tpemis  = t_bdydoc-docnum9(1).

        APPEND wa_nfe TO t_nfe.

      ELSE.

        CLEAR wa_nf.
        wa_nf-obj_key = t_bdydoc-obj_key.
        wa_nf-docnum  = v_docnum.
        wa_nf-nfnum  = t_bdydoc-nfnum.

        APPEND wa_nf TO t_nf.

      ENDIF.

      READ TABLE t_bapiret2 WITH KEY type = 'S'.
      IF sy-subrc = 0.
        t_log-type           = t_bapiret2-type.
        t_log-id             = t_bapiret2-id.
        t_log-num            = t_bapiret2-number.
        t_log-message        = t_bapiret2-message.
        t_log-message_v1     = t_bapiret2-message_v1.
        t_log-message_v2     = t_bapiret2-message_v2.
        t_log-message_v3     = t_bapiret2-message_v3.
        t_log-message_v4     = t_bapiret2-message_v4.
        APPEND t_log.
      ENDIF.

    ENDIF.

  ENDLOOP.

  DATA : v_nfenum LIKE znfeinfo-nfenum,
         v_nfnum  LIKE j_1bnfdoc-nfnum,
         v_cont   TYPE i.

  CLEAR: t_log2.

  v_cont = 1.

  IF NOT t_nfe IS INITIAL.

    LOOP AT t_nfe INTO wa_nfe.

      INSERT INTO znfeinfo VALUES wa_nfe.
      CLEAR: v_nfenum.

      WHILE NOT ( v_nfenum = wa_nfe-nfenum ) AND ( sy-subrc = 0 ) AND ( v_cont <= 3 ).
        IF wa_nfe-docstat IS INITIAL OR wa_nfe-docstat = ' '.
          wa_nfe-docstat = '1'.
        ENDIF.

        UPDATE j_1bnfdoc
           SET nfenum  = wa_nfe-nfenum
               docstat = wa_nfe-docstat
         WHERE docnum = wa_nfe-docnum.

        UPDATE j_1bnfe_active
           SET docsta = wa_nfe-docstat
               code   = wa_nfe-code
               nfnum9 = wa_nfe-nfenum
               cdv    = wa_nfe-cdv
               scssta = wa_nfe-scssta
               tpemis = wa_nfe-tpemis
         WHERE docnum = wa_nfe-docnum.

        SELECT SINGLE nfenum INTO v_nfenum
          FROM j_1bnfdoc
         WHERE docnum = wa_nfe-docnum.

        IF sy-subrc NE 0.
          IF v_cont = 3.
            t_log2-obj_key        = wa_nfe-obj_key.
            t_log2-interface      = 5.
            t_log2-dt_atualizacao = sy-datum.
            t_log2-hr_atualizacao = sy-uzeit.
            t_log2-type           = 'E'.
            t_log2-id             = '8B'.
            t_log2-num            = 0.
            CONCATENATE 'NFe numero' wa_nfe-nfenum 'com documento' wa_nfe-docnum 'nao criado!' INTO t_log2-message SEPARATED BY space.
            t_log2-message_v1     = ''.
            t_log2-message_v2     = v_nfenum.

            APPEND t_log2.

            DELETE t_log[] WHERE obj_key = wa_nfe-obj_key AND type = 'S'.
          ENDIF.

          WAIT UP TO 2 SECONDS.
          v_cont = v_cont + 1.

        ENDIF.

      ENDWHILE.

    ENDLOOP.

  ENDIF.

  "Notas  não eletronicas
  v_cont = 1.

  IF NOT t_nf IS INITIAL.

    LOOP AT t_nf INTO wa_nf.

      "INSERT INTO ZNFEINFO VALUES WA_NFE.
      CLEAR: v_nfnum.

      WHILE NOT ( v_nfnum = wa_nf-nfnum ) AND ( sy-subrc = 0 ) AND ( v_cont <= 3 ).

        SELECT SINGLE nfnum INTO v_nfnum
          FROM j_1bnfdoc
         WHERE docnum = wa_nf-docnum.

        IF sy-subrc NE 0.
          IF v_cont = 3.
            t_log2-obj_key        = wa_nf-obj_key.
            t_log2-interface      = 5.
            t_log2-dt_atualizacao = sy-datum.
            t_log2-hr_atualizacao = sy-uzeit.
            t_log2-type           = 'E'.
            t_log2-id             = '8B'.
            t_log2-num            = 0.
            CONCATENATE 'NF numero' wa_nf-nfnum 'com documento' wa_nf-docnum 'nao criado!' INTO t_log2-message SEPARATED BY space.
            t_log2-message_v1     = ''.
            t_log2-message_v2     = v_nfnum.

          ENDIF.

          APPEND t_log2.

          DELETE t_log[] WHERE obj_key = wa_nf-obj_key AND type = 'S'.

          WAIT UP TO 2 SECONDS.
          v_cont = v_cont + 1.

        ENDIF.

      ENDWHILE.

    ENDLOOP.

  ENDIF.

  DATA: lv_rfc TYPE rfcdest.
  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

  IF t_log[] IS NOT INITIAL.
    "if v_cont < 3.
* Enviar log para o Sigam Sucesso
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        OUTRETURN = T_LOG.
    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = t_log.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = t_log.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    COMMIT WORK.
  ENDIF.

  IF t_log2[] IS NOT INITIAL.
* Enviar log para o Sigam
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = t_log2.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = t_log2.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = t_log2.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    COMMIT WORK.

  ENDIF.

ENDFORM.                    "processa_nf
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_REG_LIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_reg_lido .

  LOOP AT t_bdydoc.
    UPDATE zmme_nf_sigam SET zrg_atualizado = 'S' WHERE obj_key = t_bdydoc-obj_key.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " PROCESSA_REG_LIDO
