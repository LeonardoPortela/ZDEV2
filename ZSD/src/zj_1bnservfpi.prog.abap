***INCLUDE J_1BNFPI.
************************************************************************
*    data required for NF print                                        *
************************************************************************


*... tax types ........................................................*

  TABLES: j_1baj.                      "tax types


*======================================================================*
*  forms for SAPscript print (NF specific)                             *
*======================================================================*

*&---------------------------------------------------------------------*
*&       FORM PRINT_NOTA_FISCAL                                        *
*&---------------------------------------------------------------------*
*       prints the nota fiscal via SAPscript                           *
*                                                                      *
*   Assumptions:  -  NF is already in internal WK_xxx tables           *
*                 -  SAPscript form is already opened                  *
*                                                                      *
*----------------------------------------------------------------------*


*
  FORM print_nota_fiscal.

*... table: terms of payment ..........................................*

*    TABLES: t052, t052s.

*... table: terms of payment - texts ..................................*

    DATA: BEGIN OF ztext OCCURS 4.
            INCLUDE STRUCTURE ttext.
    DATA: END OF ztext.

*... internal table: terms of payment - holdback/retainage ............*

    DATA: int_t052s LIKE t052s OCCURS 0 WITH HEADER LINE.

*... data for terms of payment ........................................*

    DATA: t052slines(2) TYPE n,
          rate          LIKE j_1bprnffa-ratpz1,
          text1         LIKE j_1bprnffa-txt11,
          text2         LIKE j_1bprnffa-txt12,
          text3         LIKE j_1bprnffa-txt13,
          text4         LIKE j_1bprnffa-txt14.

*... data for text and cfop determination .............................*

    DATA: seqnum        LIKE j_1bprnftx-seqnum,  "sequence number of text
          message       LIKE j_1bprnftx-message, "text
          old_seqnum    LIKE j_1bprnftx-seqnum,  "last sequence number
          istart        LIKE sy-tabix.
    DATA: cfop_character1(1)  TYPE c,    "one byte of CFOP
          cfop_character2(1)  TYPE c,    "one byte of CFOP
          cfop_position       LIKE sy-index, "position in CFOP
          cfop_char10(10)      TYPE c.
    DATA:
          BEGIN OF s_cfop_char10,
          s0(1),
          s1(1),
          s2(1),
          s3(1),
          s4(1),
          s5(1),
          s6(1),
          s7(1),
          s8(1),
          s9(1),
          END OF s_cfop_char10.
    DATA:
           BEGIN OF s_j_1bprnfhd_cfop,
           s0(1),
           s1(1),
           s2(1),
           s3(1),
           s4(1),
           s5(1),
           s6(1),
           s7(1),
           s8(1),
           s9(1),
           END OF s_j_1bprnfhd_cfop.



*... work fields to read issuer information ...........................*
    DATA: BEGIN OF issuer,
            partner_type     LIKE j_1bnfnad-partyp,
            partner_id       LIKE j_1bnfnad-parid,
            partner_function LIKE j_1bnfnad-parvw,
          END   OF issuer.

*... work fields to read destination information ......................*
    DATA: BEGIN OF destination,
            partner_type     LIKE j_1bnfnad-partyp,
            partner_id       LIKE j_1bnfnad-parid,
            partner_function LIKE j_1bnfnad-parvw,
          END   OF destination.

*... table with subtotals per taxsituation and ICMS taxrate, only .....*
*... used if j_1bbranch-single = ' ' (this branch is allowed to .......*
*... print NF's that consist of more than one page)....................*

    DATA: BEGIN OF inter_total_table OCCURS 0,
            matorg    LIKE j_1bprnfli-matorg,
            taxsit    LIKE j_1bprnfli-taxsit,
            icmsrate  LIKE j_1bprnfli-icmsrate,
            condensed TYPE c,
            nfnett     LIKE j_1bprnfli-nfnett,
          END OF inter_total_table.

*... help fields for handling of table inter_total_table ..............*

    DATA: tabix    LIKE sy-tabix,
          totlines(2) TYPE n.

    CLEAR:    j_1bprnfhd,                  "document header
              j_1bprnfis,                  "issuer data
              j_1bprnffa,                  "fatura data
              j_1bprnfli,                  "line item data
              j_1bprnfst,                  "subtotals across pages
              j_1bprnftr,                  "carrier data
              j_1bprnfre,                  "bill-to party data
              j_1bprnfrg,                  "payer data
              j_1bprnftx,                  "texts
              j_1bprnfde.                  "destination data

    DATA:     cfop_version      TYPE j_1bcfop_ver,         " note 593218
              item_cfop_long    TYPE j_1bcfop_long,
              cfop_length       TYPE j_1bcfop_len,         " note 593218
              extension_length  TYPE j_1bcfop_len,
*              reallength        TYPE j_1bcfop_len,
              defaulttext       TYPE j_1bcfop_txtdef,
              cfop_lines        TYPE n,                    " note 593218
              issuer_region     TYPE regio,                " note 593218
              encoded_cfop      TYPE j_1bcfop_long.

*----------------------------------------------------------------------*
*    read tax types into internal buffer table                         *
*----------------------------------------------------------------------*

    SELECT * FROM j_1baj INTO TABLE tax_types ORDER BY PRIMARY KEY.

*----------------------------------------------------------------------*
*    fill header data into communication structure                     *
*----------------------------------------------------------------------*

    MOVE-CORRESPONDING wk_header TO j_1bprnfhd.

    READ TABLE wk_item_tax
      WITH KEY taxtyp = 'ISSE'.

    WRITE wk_item_tax-rate TO v_iss_rate.
    WRITE wk_item_tax-taxval TO v_iss_val.

*---> determine CFOP length, extension and deafulttext from version
*---> table
    PERFORM get_cfop_length  USING wk_header-bukrs
                                   wk_header-branch
                                   wk_header-pstdat
                          CHANGING cfop_version     " BOI note 593218
                                   cfop_length
                                   extension_length
                                   defaulttext
                                   issuer_region.

    MOVE cfop_length TO j_1bprnfhd-cfop_len.
*... fill header CFOP .................................................*

    DATA: BEGIN OF wk_cfop OCCURS 0,
      key(6)           TYPE c,
      char6(6)         TYPE c,
      dupl_text_indic  TYPE c,
      text(50)         TYPE c.
    DATA: END OF wk_cfop.
    DATA: help_cfop(6)    TYPE c,
          default_cfop(6) TYPE c,
          lv_tabix        TYPE sytabix.


    LOOP AT wk_item.
      WRITE wk_item-cfop  TO help_cfop.
      help_cfop = help_cfop(cfop_length).
      CASE extension_length.
        WHEN 1.
          IF ( wk_item-cfop+1(3) = '991' OR wk_item-cfop+1(3) = '999' )
                                               AND issuer_region = 'SP'.
            CONCATENATE help_cfop '.' wk_item-cfop+3(1) INTO help_cfop.
          ENDIF.
        WHEN 2.
          IF wk_item-cfop+1(2) = '99' AND issuer_region = 'SC'.
            CONCATENATE help_cfop '.' wk_item-cfop+3(2) INTO help_cfop.
          ENDIF.
      ENDCASE.

      READ TABLE wk_cfop WITH KEY key = help_cfop.
      lv_tabix = sy-tabix.
      IF sy-subrc <> 0.  " new CFOP on this NF: append this CFOP to list
        wk_cfop-char6  =  wk_item-cfop.
        wk_cfop-key    =  help_cfop.
        SELECT SINGLE * FROM j_1bagnt   WHERE spras   = nast-spras
                                          AND version = cfop_version
                                          AND cfop    = wk_item-cfop.
        IF sy-subrc = 0.
          wk_cfop-text = j_1bagnt-cfotxt.
          APPEND wk_cfop.
        ELSE.
          encoded_cfop = wk_item-cfop.
          IF encoded_cfop(1) CA                    " BOI note 593218-470
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[-<>=!?]'.
            WRITE wk_item-cfop  TO encoded_cfop.
            REPLACE '/' IN encoded_cfop WITH ' '.
            CONDENSE encoded_cfop NO-GAPS.
          ELSE.
            PERFORM encoding_cfop CHANGING encoded_cfop.
          ENDIF.                                   " EOI note 593218-470
*          PERFORM ENCODING_CFOP CHANGING ENCODED_CFOP." note 593218-470
          SELECT SINGLE * FROM j_1bagnt WHERE spras = nast-spras
                                            AND version = cfop_version
                                            AND cfop    = encoded_cfop.
          IF sy-subrc = 0.
            wk_cfop-text = j_1bagnt-cfotxt.
            APPEND wk_cfop.
          ENDIF.
        ENDIF.
      ELSE. " CFOP already on list; however, could be rel. to other text
        IF wk_cfop-char6 <> wk_item-cfop AND
                                    wk_cfop-dupl_text_indic IS INITIAL.
          default_cfop      = wk_item-cfop.
          default_cfop+4(2) = defaulttext.
          SELECT SINGLE * FROM j_1bagnt WHERE spras   = nast-spras
                                          AND version = cfop_version
                                          AND cfop    = default_cfop.
          IF sy-subrc = 0.
            wk_cfop-text = j_1bagnt-cfotxt.
            wk_cfop-dupl_text_indic = 'X'.
            MODIFY wk_cfop INDEX lv_tabix.
          ELSE.
            encoded_cfop = default_cfop.
            PERFORM encoding_cfop CHANGING encoded_cfop.
            SELECT SINGLE * FROM j_1bagnt WHERE spras = nast-spras
                                            AND version = cfop_version
                                            AND cfop    = encoded_cfop.
            IF sy-subrc = 0.
              wk_cfop-text = j_1bagnt-cfotxt.
              wk_cfop-dupl_text_indic = 'X'.
              MODIFY wk_cfop INDEX lv_tabix.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE wk_cfop LINES cfop_lines.
    IF cfop_lines > 1.
      SORT wk_cfop.
      DELETE ADJACENT DUPLICATES FROM wk_cfop COMPARING key.
      LOOP AT wk_cfop.
        CONCATENATE j_1bprnfhd-cfop_text '/' wk_cfop-key wk_cfop-text
                                            INTO j_1bprnfhd-cfop_text.
        IF j_1bprnfhd-cfop_text(1) EQ '/'.
          SHIFT j_1bprnfhd-cfop_text LEFT BY 1 PLACES.
        ENDIF.
      ENDLOOP.
    ELSEIF cfop_lines = 1.      " NF with items that all have one CFOP
      MOVE wk_cfop-key  TO j_1bprnfhd-cfop.
      MOVE wk_cfop-text TO j_1bprnfhd-cfop_text.
    ENDIF.                                             " BOI note 593218
*                            CHANGING CFOP_LENGTH
*                                      EXTENSION_LENGTH
*                                      DEFAULTTEXT.
*
*    MOVE CFOP_LENGTH TO J_1BPRNFHD-CFOP_LEN.
*
**... fill header CFOP .................................................
*
*
*    READ TABLE WK_ITEM INDEX 1.
**-->  fill header CFOP with 1st item CFOP
*    WRITE WK_ITEM-CFOP TO J_1BPRNFHD-CFOP.
*
*    LOOP AT WK_ITEM FROM 2.
**    CFOP_POSITION = 0.
**    WHILE CFOP_POSITION < 5
**                  VARY CFOP_CHARACTER1 FROM J_1BPRNFHD-CFOP(1)
**                                       NEXT J_1BPRNFHD-CFOP+1(1)
**                  VARY CFOP_CHARACTER2 FROM WK_ITEM-CFOP(1)
**                                       NEXT WK_ITEM-CFOP+1(1).
**
**---> compare CFOP digits starting at 2nd one
**---> 1st one is already unique due to first_digit_check
*      CFOP_POSITION = 1.
**---> calculate length from CFOP inclusive extensions
*      REALLENGTH = CFOP_LENGTH + EXTENSION_LENGTH.
*      WRITE WK_ITEM-CFOP TO CFOP_CHAR10.
*      MOVE CFOP_CHAR10       TO S_CFOP_CHAR10.
*      MOVE J_1BPRNFHD-CFOP   TO S_J_1BPRNFHD_CFOP.
*
*      WHILE CFOP_POSITION < REALLENGTH
*                    VARY CFOP_CHARACTER1 FROM S_J_1BPRNFHD_CFOP-S1
*                                         NEXT S_J_1BPRNFHD_CFOP-S2
*                    VARY CFOP_CHARACTER2 FROM S_CFOP_CHAR10-S1
*                                         NEXT S_CFOP_CHAR10-S2.
*
*        IF CFOP_CHARACTER1 <> CFOP_CHARACTER2.
*          EXIT.
*        ENDIF.
*        CFOP_POSITION = CFOP_POSITION + 1.
*      ENDWHILE.
**   IF CFOP_POSITION <> 5.
*      IF CFOP_POSITION <> REALLENGTH.
*        MOVE '00000'     TO J_1BPRNFHD-CFOP+CFOP_POSITION.
*        MOVE DEFAULTTEXT TO  J_1BPRNFHD-CFOP+CFOP_LENGTH.
*      ENDIF.
*    ENDLOOP.
*
*    SELECT SINGLE * FROM J_1BAGNT WHERE SPRAS = NAST-SPRAS
*                                  AND CFOP    = J_1BPRNFHD-CFOP.
*
*    IF SY-SUBRC = 0.
*      J_1BPRNFHD-CFOP_TEXT = J_1BAGNT-CFOTXT.
*    ELSE.
*      ENCODED_CFOP = J_1BPRNFHD-CFOP.
*      PERFORM ENCODING_CFOP CHANGING ENCODED_CFOP.
*      SELECT SINGLE * FROM J_1BAGNT WHERE SPRAS = NAST-SPRAS
*                                    AND CFOP    = ENCODED_CFOP.
*        IF SY-SUBRC = 0.
*          J_1BPRNFHD-CFOP_TEXT = J_1BAGNT-CFOTXT.
*        ENDIF.
*    ENDIF.
*----------------------------------------------------------------------*
*    determine issuer and destination (only for test)                  *
*----------------------------------------------------------------------*
    IF wk_header-direct = '1'   AND
       wk_header-entrad = ' '.
      issuer-partner_type      = wk_header-partyp.
      issuer-partner_id        = wk_header-parid.
      issuer-partner_function  = wk_header-parvw.
      destination-partner_type = 'B'.
      destination-partner_id   = wk_header-bukrs.
      destination-partner_id+4 = wk_header-branch.
    ELSE.
      issuer-partner_type          = 'B'.
      issuer-partner_id            = wk_header-bukrs.
      issuer-partner_id+4          = wk_header-branch.
      destination-partner_type     = wk_header-partyp.
      destination-partner_id       = wk_header-parid.
      destination-partner_function = wk_header-parvw.
    ENDIF.

*----------------------------------------------------------------------*
*    read branch data (issuer)                                         *
*----------------------------------------------------------------------*

    CLEAR j_1binnad.

    CALL FUNCTION 'J_1B_NF_PARTNER_READ'
      EXPORTING
        partner_type           = issuer-partner_type
        partner_id             = issuer-partner_id
        partner_function       = issuer-partner_function
        doc_number             = wk_header-docnum
        obj_item               = wk_item
      IMPORTING
        parnad                 = j_1binnad
      EXCEPTIONS
        partner_not_found      = 1
        partner_type_not_found = 2
        OTHERS                 = 3.
    MOVE-CORRESPONDING j_1binnad TO j_1bprnfis.

*... check the sy-subrc ...............................................*
    PERFORM check_error.
    CHECK retcode IS INITIAL.

*----------------------------------------------------------------------*
*    read destination data                                             *
*----------------------------------------------------------------------*

    CLEAR j_1binnad.

    CALL FUNCTION 'J_1B_NF_PARTNER_READ'
      EXPORTING
        partner_type           = destination-partner_type
        partner_id             = destination-partner_id
        partner_function       = destination-partner_function
        doc_number             = wk_header-docnum
        obj_item               = wk_item
      IMPORTING
        parnad                 = j_1binnad
      EXCEPTIONS
        partner_not_found      = 1
        partner_type_not_found = 2
        OTHERS                 = 3.
    MOVE-CORRESPONDING j_1binnad TO j_1bprnfde.

*... check the sy-subrc ...............................................*
    PERFORM check_error.
    CHECK retcode IS INITIAL.

*----------------------------------------------------------------------*
* add branch data (issuer) - inscricao estadual do substituto tributario
*----------------------------------------------------------------------*

    SELECT SINGLE * FROM j_1bstast WHERE bukrs  = wk_header-bukrs
                                     AND branch = wk_header-branch
                                     AND txreg  = j_1bprnfde-txjcd.

    IF sy-subrc = 0.
      MOVE j_1bstast-state_insc TO j_1bprnfis-state_insc.
    ELSE.
      CLEAR j_1bprnfis-state_insc.
    ENDIF.

*----------------------------------------------------------------------*
*   read payer data (if existing)
*----------------------------------------------------------------------*

    READ TABLE wk_partner WITH KEY docnum = wk_header-docnum
                                   parvw  = 'RG'.
    IF sy-subrc = 0.
      CLEAR j_1binnad.

      CALL FUNCTION 'J_1B_NF_PARTNER_READ'
        EXPORTING
          partner_type           = wk_partner-partyp
          partner_id             = wk_partner-parid
          partner_function       = wk_partner-parvw
          doc_number             = wk_header-docnum
        IMPORTING
          parnad                 = j_1binnad
        EXCEPTIONS
          partner_not_found      = 1
          partner_type_not_found = 2
          OTHERS                 = 3.
      MOVE-CORRESPONDING j_1binnad TO j_1bprnfrg.

    ENDIF.

*----------------------------------------------------------------------*
*   read bill-to party data (if existing)
*----------------------------------------------------------------------*

    READ TABLE wk_partner WITH KEY docnum = wk_header-docnum
                                   parvw  = 'RE'.
    IF sy-subrc = 0.
      CLEAR j_1binnad.

      CALL FUNCTION 'J_1B_NF_PARTNER_READ'
        EXPORTING
          partner_type           = wk_partner-partyp
          partner_id             = wk_partner-parid
          partner_function       = wk_partner-parvw
          doc_number             = wk_header-docnum
        IMPORTING
          parnad                 = j_1binnad
        EXCEPTIONS
          partner_not_found      = 1
          partner_type_not_found = 2
          OTHERS                 = 3.
      MOVE-CORRESPONDING j_1binnad TO j_1bprnfre.

    ENDIF.

*----------------------------------------------------------------------*
*    read fatura data if the Nota Fiscal is a Nota Fiscal Fatura       *
*----------------------------------------------------------------------*

    IF wk_header-fatura = 'X'.

      IF wk_header-zterm NE space.
        SELECT * FROM t052 WHERE zterm = wk_header-zterm ORDER BY PRIMARY KEY.
          EXIT.
        ENDSELECT.

        IF t052-ztagg > '00' AND t052-ztagg LT wk_header-zfbdt+6(2).
          SELECT * FROM t052 WHERE zterm =  wk_header-zterm
                             AND   ztagg GE wk_header-zfbdt+6(2)
                             ORDER BY PRIMARY KEY.
            EXIT.
          ENDSELECT.
        ENDIF.

        IF t052-xsplt = 'X'.               "holdback/retainage

          SELECT * FROM t052s INTO TABLE int_t052s WHERE zterm = wk_header-zterm
                                                        ORDER BY PRIMARY KEY.
          DESCRIBE TABLE int_t052s LINES t052slines.

          IF t052slines > 5.
            t052slines = 5.  "max. number of holdbacks/retainages printed on NF
          ENDIF.

          DO t052slines TIMES VARYING rate  FROM j_1bprnffa-ratpz1
                                            NEXT j_1bprnffa-ratpz2
                              VARYING text2 FROM j_1bprnffa-txt12
                                            NEXT j_1bprnffa-txt22
                              VARYING text3 FROM j_1bprnffa-txt13
                                            NEXT j_1bprnffa-txt23
                              VARYING text4 FROM j_1bprnffa-txt14
                                            NEXT j_1bprnffa-txt24
                              VARYING text1 FROM j_1bprnffa-txt11
                                            NEXT j_1bprnffa-txt21.

            READ TABLE int_t052s INDEX sy-index.
            rate = int_t052s-ratpz.
            SELECT SINGLE * FROM t052 WHERE zterm = int_t052s-ratzt
                                      AND   ztagg = '00'.
            CALL FUNCTION 'FI_TEXT_ZTERM'
              EXPORTING
                i_t052  = t052
              TABLES
                t_ztext = ztext.
            LOOP AT ztext.
              CASE sy-tabix.
                WHEN 1.
                  text2 = ztext-text1.
                WHEN 2.
                  text3 = ztext-text1.
                WHEN 3.
                  text4 = ztext-text1.
                WHEN 4.
                  text1 = ztext-text1.
              ENDCASE.
            ENDLOOP.
          ENDDO.
        ELSE.                              " t052-xsplt = ' '
          CALL FUNCTION 'FI_TEXT_ZTERM'
            EXPORTING
              i_t052  = t052
            TABLES
              t_ztext = ztext.

          LOOP AT ztext.
            CASE sy-tabix.
              WHEN 1.
                j_1bprnffa-txt02 = ztext-text1.
              WHEN 2.
                j_1bprnffa-txt03 = ztext-text1.
              WHEN 3.
                j_1bprnffa-txt04 = ztext-text1.
              WHEN 4.
                j_1bprnffa-txt01 = ztext-text1.
            ENDCASE.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
* end of change --------------------------------------------------------

*----------------------------------------------------------------------*
*    read carrier data                                                 *
*----------------------------------------------------------------------*

    IF wk_header-doctyp NE '2'.          "no carrier for Complementars

      READ TABLE wk_partner WITH KEY docnum = wk_header-docnum
                                     parvw  = 'SP'.
      IF sy-subrc = 0.

        CLEAR j_1binnad.
        CALL FUNCTION 'J_1B_NF_PARTNER_READ'
          EXPORTING
            partner_type           = wk_partner-partyp
            partner_id             = wk_partner-parid
            partner_function       = wk_partner-parvw
            doc_number             = wk_header-docnum
          IMPORTING
            parnad                 = j_1binnad
          EXCEPTIONS
            partner_not_found      = 1
            partner_type_not_found = 2
            OTHERS                 = 3.
        MOVE-CORRESPONDING j_1binnad TO j_1bprnftr.
      ENDIF.

    ENDIF.          "no carrier for Complementars

*----------------------------------------------------------------------*
*    read reference NF                                                 *
*----------------------------------------------------------------------*
    IF j_1bprnfhd-docref <> space.
      SELECT SINGLE * FROM j_1bnfdoc INTO *j_1bnfdoc
               WHERE docnum = j_1bprnfhd-docref.
      j_1bprnfhd-nf_docref = *j_1bnfdoc-nfnum.
      j_1bprnfhd-nf_serref = *j_1bnfdoc-series.
      j_1bprnfhd-nf_subref = *j_1bnfdoc-subser.
      j_1bprnfhd-nf_datref = *j_1bnfdoc-docdat.
    ENDIF.


*----------------------------------------------------------------------*
*    get information about form                                        *
*----------------------------------------------------------------------*

*    SELECT SINGLE * FROM J_1BB2 WHERE BUKRS  = J_1BPRNFHD-BUKRS
*                                  AND BRANCH = J_1BPRNFHD-BRANCH
*                                  AND FORM   = J_1BPRNFHD-FORM.
    DATA: print_conf TYPE j_1bb2.                      " BOI note 743361

    CALL FUNCTION 'J_1BNF_GET_PRINT_CONF'
      EXPORTING
        headerdata = wk_header
      IMPORTING
        print_conf = print_conf
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.               " EOI note 743361

    PERFORM check_error.
    CHECK retcode IS INITIAL.

*----------------------------------------------------------------------*
*    write texts to TEXTS window                                       *
*----------------------------------------------------------------------*

*    ISTART = J_1BB2-TOTLIH.                          " note note 743361
    istart = print_conf-totlih.                       " note note 743361
    IF istart > 16.   "maximum number of fields in J_1BPRNFTX
      istart = 16.
    ENDIF.

*... fill fields in communication structure J_1BPRNFTX ................*

    DO istart TIMES VARYING seqnum  FROM j_1bprnftx-seqnum01
                                    NEXT j_1bprnftx-seqnum02
                    VARYING message FROM j_1bprnftx-message01
                                    NEXT j_1bprnftx-message02.

      READ TABLE wk_header_msg INDEX sy-index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      seqnum  = wk_header_msg-seqnum.
      message = wk_header_msg-message.

    ENDDO.

* Mateus - Inicio
*******----------------------------------------------------------------------*
*******    write header of main window (including subtotals on top)          *
*******----------------------------------------------------------------------*
******
******    CALL FUNCTION 'WRITE_FORM'
******      EXPORTING
******        element = 'HEADER'
******        window  = 'MAIN'
******        type    = 'TOP'
******      EXCEPTIONS
******        OTHERS  = 01.
******
*******----------------------------------------------------------------------*
*******    write subtotals at end of page                                    *
*******----------------------------------------------------------------------*
******
******    CALL FUNCTION 'WRITE_FORM'
******      EXPORTING
******        element = 'FOOTER'
******        window  = 'SUBTOTAL'
******      EXCEPTIONS
******        OTHERS  = 01.

    DATA: vl_vgbel TYPE vbrp-vgbel,
          tl_lines TYPE TABLE OF tline,
          sl_lines TYPE tline,
          vl_name  TYPE thead-tdname.

    CLEAR: v_texto1,
           v_texto2,
           v_texto3.

    READ TABLE wk_item INDEX 1.

    SELECT SINGLE vgbel
      FROM vbrp
      INTO vl_vgbel
    WHERE  vbeln EQ wk_item-refkey.
    vl_name = vl_vgbel.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0001'
        language                = sy-langu
        name                    = vl_name
        object                  = 'VBBK'
      TABLES
        lines                   = tl_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF NOT tl_lines[] IS INITIAL.
      READ TABLE tl_lines INTO sl_lines INDEX 1.
      IF sy-subrc IS INITIAL.
        v_texto1 = sl_lines-tdline.
      ENDIF.
      READ TABLE tl_lines INTO sl_lines INDEX 2.
      IF sy-subrc IS INITIAL.
        v_texto2 = sl_lines-tdline.
      ENDIF.
      READ TABLE tl_lines INTO sl_lines INDEX 3.
      IF sy-subrc IS INITIAL.
        v_texto3 = sl_lines-tdline.
      ENDIF.
    ENDIF.

    CONCATENATE vl_vgbel
                wk_item-itmnum
           INTO vl_name.

    REFRESH tl_lines.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0001'
        language                = sy-langu
        name                    = vl_name
        object                  = 'VBBP'
      TABLES
        lines                   = tl_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF NOT tl_lines[] IS INITIAL.
      READ TABLE tl_lines INTO sl_lines INDEX 1.
      IF sy-subrc IS INITIAL.
        IF v_texto1 IS INITIAL.
          v_texto1 = sl_lines-tdline.
        ELSE.
          IF v_texto2 IS INITIAL.
            v_texto2 = sl_lines-tdline.
          ELSE.
            IF v_texto3 IS INITIAL.
              v_texto3 = sl_lines-tdline.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE tl_lines INTO sl_lines INDEX 2.
      IF sy-subrc IS INITIAL.
        IF v_texto2 IS INITIAL.
          v_texto2 = sl_lines-tdline.
        ELSE.
          IF v_texto3 IS INITIAL.
            v_texto3 = sl_lines-tdline.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE tl_lines INTO sl_lines INDEX 3.
      IF sy-subrc IS INITIAL.
        IF v_texto3 IS INITIAL.
          v_texto3 = sl_lines-tdline.
        ENDIF.
      ENDIF.
    ENDIF.

    vl_name = wk_item-refkey.
    REFRESH tl_lines.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0002'
        language                = sy-langu
        name                    = vl_name
        object                  = 'VBBK'
      TABLES
        lines                   = tl_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF NOT tl_lines[] IS INITIAL.
      READ TABLE tl_lines INTO sl_lines INDEX 1.
      IF sy-subrc IS INITIAL.
        IF v_texto1 IS INITIAL.
          v_texto1 = sl_lines-tdline.
        ELSE.
          IF v_texto2 IS INITIAL.
            v_texto2 = sl_lines-tdline.
          ELSE.
            IF v_texto3 IS INITIAL.
              v_texto3 = sl_lines-tdline.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE tl_lines INTO sl_lines INDEX 2.
      IF sy-subrc IS INITIAL.
        IF v_texto2 IS INITIAL.
          v_texto2 = sl_lines-tdline.
        ELSE.
          IF v_texto3 IS INITIAL.
            v_texto3 = sl_lines-tdline.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE tl_lines INTO sl_lines INDEX 3.
      IF sy-subrc IS INITIAL.
        IF v_texto3 IS INITIAL.
          v_texto3 = sl_lines-tdline.
        ENDIF.
      ENDIF.
    ENDIF.

    v_cfop_text = j_1bprnfhd-cfop_text.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window = 'HEADER'.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window = 'DATA'.

    CLEAR j_1bprnfhd-nftot.
* Mateus - Final
*----------------------------------------------------------------------*
*    write item information (MAIN window)                              *
*----------------------------------------------------------------------*

    LOOP AT wk_item.

      READ TABLE wk_item_add WITH KEY docnum = wk_item-docnum
                                itmnum = wk_item-itmnum.

      CLEAR j_1bprnfli.
      MOVE-CORRESPONDING wk_item TO j_1bprnfli.
      MOVE-CORRESPONDING wk_item_add TO j_1bprnfli.

*... fill text reference ..............................................*

* Mateus - Inicio
*****      LOOP AT wk_refer_msg WHERE itmnum = wk_item-itmnum.
*****        REPLACE '  ' WITH wk_refer_msg-seqnum INTO j_1bprnfli-text_ref.
*****        REPLACE ' '  WITH ','                 INTO j_1bprnfli-text_ref.
*****      ENDLOOP.
*****      REPLACE ', ' WITH '  ' INTO j_1bprnfli-text_ref.
*****
******... fill service code on line item level .............................*
*****      IF NOT wk_item-tmiss IS INITIAL.
*****        LOOP AT wk_item_tax WHERE itmnum = wk_item-itmnum.
*****          READ TABLE tax_types WITH KEY taxtyp = wk_item_tax-taxtyp.
*****          IF tax_types-taxgrp = 'ISSP'.
*****            MOVE wk_item_tax-servtype_out TO j_1bprnfli-servtype_out.
*****          ELSEIF tax_types-taxgrp = 'ISSS'.
*****            MOVE wk_item_tax-servtype_out TO j_1bprnfli-servtype_in.
*****          ENDIF.
*****        ENDLOOP.
*****      ENDIF.
* Mateus - Final

*... determine totals per ICMS taxrate and taxsituation and update ....*
*... table inter_total_table if the branch is allowed to print ........*
*... NF's consisting of more than one page ............................*

      CLEAR j_1bbranch.
      SELECT SINGLE * FROM j_1bbranch  WHERE bukrs  = j_1bprnfhd-bukrs
                                       AND   branch = j_1bprnfhd-branch.

      IF j_1bbranch-single = ' '.        "more than one page allowed

        CLEAR inter_total_table.
        READ TABLE inter_total_table WITH KEY
             matorg    = j_1bprnfli-matorg
             taxsit    = j_1bprnfli-taxsit
             icmsrate = j_1bprnfli-icmsrate
             BINARY SEARCH.

        tabix = sy-tabix.

        IF sy-subrc NE '0'.
          MOVE-CORRESPONDING j_1bprnfli TO inter_total_table.
          INSERT inter_total_table INDEX tabix.
        ELSE.
          inter_total_table-nfnett = inter_total_table-nfnett
                                          + j_1bprnfli-nfnett.
          inter_total_table-condensed = 'X'.
          MODIFY inter_total_table INDEX tabix.
        ENDIF.

      ENDIF.                             "j_1bbranch-single = ' '

*... move line data to SAPScript ......................................*
      WRITE j_1bprnfli-taxsit TO v_sittrib.
      IF j_1bprnfli-matnr+12 = space.
        j_1bprnfli-matnr+12 = '000000'.
      ENDIF.
      CONCATENATE j_1bprnfli-matnr+12 j_1bprnfli-maktx
             INTO v_descr SEPARATED BY space.
      CONCATENATE j_1bprnfli-matorg v_sittrib INTO v_sittrib.
      v_unit = j_1bprnfli-nfpri.
      v_ipirate = j_1bprnfli-ipirate.
      v_icmsrate = j_1bprnfli-icmsrate.
      WRITE v_unit TO v_vrunit.
      WRITE j_1bprnfli-nfqty TO v_quant.
      WRITE j_1bprnfli-nfnet TO v_vrtot.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'LINE'
          window  = 'MAIN'
        EXCEPTIONS
          OTHERS  = 01.

*... add subtotals (begin of page) ....................................*

* begin change 21.07.97: statistical IPI
      IF j_1bprnfli-ipistat = ' '.
        j_1bprnfst-ipival    = j_1bprnfst-ipival + j_1bprnfli-ipival.
      ENDIF.
* end change 21.07.97: statistical IPI
      j_1bprnfst-nfnett    = j_1bprnfst-nfnett  + j_1bprnfli-nfnett.

*... check the sy-subrc ...............................................*

      PERFORM check_error.
      CHECK retcode IS INITIAL.

      j_1bprnfhd-nftot = j_1bprnfli-netwr + j_1bprnfhd-nftot.
    ENDLOOP.
    CHECK retcode IS INITIAL.

* Mateus - Inicio
    WRITE j_1bprnfhd-nftot TO v_nftot.

    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        amount    = j_1bprnfhd-nftot
        currency  = 'BRL'
        language  = sy-langu
      IMPORTING
        in_words  = wa_in_words
      EXCEPTIONS
        not_found = 1
        too_large = 2
        OTHERS    = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF wa_in_words-decimal EQ '000'.
      CONCATENATE wa_in_words-word
                  'REAIS'
        INTO v_vlext SEPARATED BY space.
    ELSE.
      CONCATENATE wa_in_words-word
                  'REAIS E'
                  wa_in_words-decword
                  'CENTAVOS'
        INTO v_vlext SEPARATED BY space.
    ENDIF.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window = 'IMPOSTOS'.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window = 'TOTAL'.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window = 'EXTENSO'.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window = 'OBS'.

* Mateus - Final

*----------------------------------------------------------------------*
* fill main window up with totals per ICMS taxrate and taxsituation if
* - the branch is allowed to print NF's consisting of more than one page
* - there are different lines on the NF with the same taxsituation and/
*   or ICMS taxrate
* - there is more than one combination of ICMS taxrate and taxsituation
*   on this NF
*----------------------------------------------------------------------*

    IF j_1bbranch-single = ' '.          "more than one page allowed
      DESCRIBE TABLE inter_total_table LINES totlines.
      IF totlines > 1.                   "more than one taxsituation or
        "icms rate on the NF

        LOOP AT inter_total_table.
          IF inter_total_table-condensed = 'X'.
            MOVE-CORRESPONDING inter_total_table TO j_1bprnfli.

*... move line data to SAPScript ......................................*

            CALL FUNCTION 'WRITE_FORM'
              EXPORTING
                element = 'INTERTOTALS'
                window  = 'MAIN'
              EXCEPTIONS
                OTHERS  = 01.
*... check the sy-subrc ...............................................*
            PERFORM check_error.
            CHECK retcode IS INITIAL.
          ENDIF.                      "inter_total_table-condensed = 'X'
        ENDLOOP.                         "at inter_total_table

      ENDIF.
    ENDIF.                               "j_1bbranch-single = ' '

*----------------------------------------------------------------------*
*    fill main window up with remaining text lines                     *
*----------------------------------------------------------------------*

    istart = istart + 1.
    CLEAR: old_seqnum,
           j_1bprnftx-seqnum_cha.
*    loop at wk_header_msg from istart.
*
**... fill communication structure .....................................*
*
*      j_1bprnftx-seqnum  = wk_header_msg-seqnum.
*      j_1bprnftx-message = wk_header_msg-message.
*      if old_seqnum <> wk_header_msg-seqnum.
*        old_seqnum = wk_header_msg-seqnum.
*        j_1bprnftx-seqnum_cha = wk_header_msg-seqnum.
*      else.
*        clear j_1bprnftx-seqnum_cha.
*      endif.
*
**... move line data to SAPScript ......................................*
*
*      call function 'WRITE_FORM'
*        exporting
*          element = 'TEXTS'
*          window  = 'MAIN'
*        exceptions
*          others  = 01.
*
**... check the sy-subrc ...............................................*
*      perform check_error.
*      check retcode is initial.
*
*    endloop.


* Mateus - Inicio
*****    LOOP AT wk_header_msg.
*****      CASE sy-tabix.
*****        WHEN 1.
*****          j_1bprnftx-message01 = wk_header_msg-message.
*****        WHEN 2.
*****          j_1bprnftx-message02 = wk_header_msg-message.
*****        WHEN 3.
*****          j_1bprnftx-message03 = wk_header_msg-message.
*****        WHEN 4.
*****          j_1bprnftx-message04 = wk_header_msg-message.
*****        WHEN 5.
*****          j_1bprnftx-message05 = wk_header_msg-message.
*****        WHEN 6.
*****          j_1bprnftx-message06 = wk_header_msg-message.
*****        WHEN 7.
*****          j_1bprnftx-message07 = wk_header_msg-message.
*****        WHEN 8.
*****          j_1bprnftx-message08 = wk_header_msg-message.
*****      ENDCASE.
*****    ENDLOOP.
*****
*****    CALL FUNCTION 'WRITE_FORM'
*****      EXPORTING
******          element = 'TEXTS'
*****        window  = 'TEXTS'
*****      EXCEPTIONS
*****        OTHERS  = 01.
*****
******----------------------------------------------------------------------*
******    fill totals into communication structure (only printed on         *
******    last page, therefore filled at the end after the last print)      *
******----------------------------------------------------------------------*
*****
*****    MOVE-CORRESPONDING wk_header_add TO j_1bprnfhd.
*****
******----------------------------------------------------------------------*
******    delete subtotal window on last page                               *
******----------------------------------------------------------------------*
*****
*****    CALL FUNCTION 'WRITE_FORM'
*****      EXPORTING
*****        function = 'DELETE'
*****        element  = 'FOOTER'
*****        window   = 'SUBTOTAL'
*****      EXCEPTIONS
*****        OTHERS   = 01.
* Mateus - Final

  ENDFORM.                    "PRINT_NOTA_FISCAL
*&---------------------------------------------------------------------*
*&      Form  get_cfop_length
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BUKRS  text
*      -->P_BRANCH  text
*      -->P_PSTDAT  text
*      <--P_CLENGTH  text
*      <--P_ELENGTH  text
*----------------------------------------------------------------------*
  FORM get_cfop_length USING    p_bukrs
                                p_branch
                                p_pstdat
                       CHANGING p_version         " note 593218
                                p_clength
                                p_elength
                                p_text
                                p_region.         " note 593218

    DATA: lv_adress   TYPE addr1_val.

    CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
      EXPORTING
        bukrs             = p_bukrs
        branch            = p_branch
      IMPORTING
        address1          = lv_adress
      EXCEPTIONS
        branch_not_found  = 1
        address_not_found = 2
        company_not_found = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    p_region = lv_adress-region.                   " note 593218

    CALL FUNCTION 'J_1B_CFOP_GET_VERSION'
      EXPORTING
*   LAND1                   =
        region                  = lv_adress-region
        date                    = p_pstdat
     IMPORTING
        version                 = p_version        " note 593218
        extension               = p_elength
        cfoplength              = p_clength
        txtdef                  = p_text
     EXCEPTIONS
        date_missing            = 1
        version_not_found       = 2
        OTHERS                  = 3
              .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDFORM.                             " get_cfop_length

*&---------------------------------------------------------------------*
*&      Form  ENCODING_CFOP
*&---------------------------------------------------------------------*
*       encode the CFOP
*      51234   =>  51234
*      5123A   =>  5123A
*      512345  =>  512345
*      51234A  =>  51234A
*      5123B4  =>  5123B4
*      5123BA  =>  5123BA
*----------------------------------------------------------------------*
  FORM encoding_cfop  CHANGING p_cfop.

    DATA: len(1) TYPE n,
          helpstring(60) TYPE c,
          d TYPE i.

    helpstring =
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[-<>=!?]'. "#EC NOTEXT

    len = STRLEN( p_cfop ).
    IF len = 6.
      CASE p_cfop(1).
        WHEN 1. d = 0.
        WHEN 2. d = 1.
        WHEN 3. d = 2.
        WHEN 5. d = 3.
        WHEN 6. d = 4.
        WHEN 7. d = 5.
      ENDCASE.
      d = d * 10 + p_cfop+1(1).
      SHIFT helpstring BY d PLACES.
      MOVE helpstring(1) TO p_cfop(1).
      p_cfop+1(4) = p_cfop+2(4).
      CLEAR p_cfop+5(1).
    ENDIF.

  ENDFORM.                    " ENCODING_CFOP
