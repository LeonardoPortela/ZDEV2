FUNCTION z_1b_nf_document_select_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(COMPANY) LIKE  J_1BNFDOC-BUKRS DEFAULT SPACE
*"     VALUE(BRANCH) LIKE  J_1BNFDOC-BRANCH DEFAULT SPACE
*"     VALUE(NF_NUMBER) LIKE  J_1BNFDOC-NFNUM
*"     VALUE(MODEL) LIKE  J_1BNFDOC-MODEL
*"     VALUE(SERIES) LIKE  J_1BNFDOC-SERIES
*"     VALUE(SUBSERIES) LIKE  J_1BNFDOC-SUBSER
*"     VALUE(PARTNER_ID) LIKE  J_1BNFDOC-PARID DEFAULT SPACE
*"     VALUE(PARTNER_TYPE) LIKE  J_1BNFDOC-PARTYP DEFAULT SPACE
*"     REFERENCE(CHECK_REF_MD) TYPE  C DEFAULT SPACE
*"     VALUE(DATE) TYPE  D OPTIONAL
*"     REFERENCE(I_NFEFLAG) TYPE  J_1BNFE OPTIONAL
*"     REFERENCE(I_NFNUM9) TYPE  J_1BDOCNUM9 OPTIONAL
*"     REFERENCE(I_DOCNUM_NP) TYPE  ZSDT0001NT-DOCNUM_NP OPTIONAL
*"     REFERENCE(I_NR_CHAVE_NFE) TYPE  ZSDT0001NT-NR_CHAVE_NFE OPTIONAL
*"     REFERENCE(I_ID_FORNECEDOR) TYPE  ZSDT0001NT-ID_FORNECEDOR
*"       OPTIONAL
*"     REFERENCE(I_NR_NOTA2) TYPE  ZSDT0001NT-NR_NOTA OPTIONAL
*"     REFERENCE(I_NR_FORNECEDOR_IE) TYPE  ZSDT0001NT-NR_FORNECEDOR_IE
*"       OPTIONAL
*"     REFERENCE(I_ID_ENTRADA) TYPE  ZSDT0001NT-ID_ENTRADA OPTIONAL
*"     REFERENCE(I_ID_BUKRS) TYPE  BUKRS OPTIONAL
*"  EXPORTING
*"     VALUE(DOC_NUMBER) LIKE  J_1BNFDOC-DOCNUM
*"  EXCEPTIONS
*"      DOCUMENT_NOT_FOUND
*"      DOC_WITH_SAME_YEAR_FOUND
*"      DOC_WITH_DIFF_YEAR_FOUND
*"      TOO_MANY_DOCUMENTS_FOUND
*"----------------------------------------------------------------------
*
* changes in the access to J_1BNFDOC for references for mixed   "1175538
* scenarios (NF refers to NF-e or NF-e to NF).                  "1175538
* Decision about which kind of document (NF or NF-e)            "1175538
* is read is done based on the filled NF_NUMBER(normal NF) or   "1175538
* I_NFNUM9 (NF-e Number). The fields are set in function        "1175538
* J_1B_NF_NUMBER_SEPARATE when called for reference numbers and "1175538
* parameter I_SET2NUMBERS is set to X                           "1175538
*
  DATA: nfdocs       LIKE j_1bnfdoc OCCURS 0 WITH HEADER LINE,
        t_nflin      LIKE j_1bnflin OCCURS 0 WITH HEADER LINE,
        who          LIKE j_1bnfdoc-parid,
        ref_number   LIKE bkpf-xblnr,
        ref_found(1) TYPE c    VALUE space,
        lc_docnum_np TYPE j_1bdocnum,
        lc_objkey_np TYPE awkey.

* Note 550160: Indicates matching date of existing NF's
  DATA: lv_exist     TYPE i.
  DATA: lv_subrc_nf  TYPE sy-subrc.                         "1175538

*-CS2021000183-#71105-26.04.2022-JT-inicio
  FREE: lc_docnum_np,
        lc_objkey_np.

*----------------------------------------------------
*-Checae se é Nota própria
*----------------------------------------------------
  SELECT ct_nota, tp_pessoa
    FROM zsdt0001te
    INTO @DATA(w_zsdt0001te)
      UP TO 1 ROWS
   WHERE id_entrada = @i_id_entrada
     AND id_empresa = @i_id_bukrs.
  ENDSELECT.

  IF sy-subrc = 0.
    SELECT form
      INTO @DATA(l_form)
      FROM j_1baa
        UP TO 1 ROWS
     WHERE nftype = @w_zsdt0001te-ct_nota.
    ENDSELECT.

    IF l_form IS NOT INITIAL.
      IF i_nr_chave_nfe IS NOT INITIAL.
        SELECT     docnum_ref    objkey_np
          INTO (lc_docnum_np, lc_objkey_np)
          FROM zib_nfe_forn
            UP TO 1 ROWS
         WHERE nu_chave  = i_nr_chave_nfe.
        ENDSELECT.
      ELSE.
        SELECT stcd2
          INTO @DATA(l_stcd2)
          FROM lfa1
            UP TO 1 ROWS
         WHERE lifnr = @i_id_fornecedor.
        ENDSELECT.

        SELECT     docnum_ref    objkey_np
          INTO (lc_docnum_np, lc_objkey_np)
          FROM zib_nfe_forn
            UP TO 1 ROWS
         WHERE nu_chave_cnpj   = l_stcd2
           AND dt_emissao      = date
           AND nu_chave_numero = i_nr_nota2
           AND nu_ie           = i_nr_fornecedor_ie.
        ENDSELECT.
      ENDIF.

      IF lc_docnum_np IS INITIAL.
        RAISE document_not_found.
      ELSE.
        SELECT SINGLE *
          INTO @DATA(w_active)
          FROM j_1bnfe_active
         WHERE docnum = @lc_docnum_np.

        IF ( w_active-docsta = 1 AND w_active-scssta = 2 ) OR
           ( w_active-docsta = 2 AND w_active-scssta = 4 ).
          RAISE document_not_found.
        ELSE.
          doc_number = lc_docnum_np.
        ENDIF.
      ENDIF.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2021000183-#71105-26.04.2022-JT-fim

  IF NOT check_ref_md IS INITIAL.
* check if reference document does contain a MD line

    IF date IS INITIAL.                                     "774273
*      IF i_nfeflag IS INITIAL.                              "1175538
*        SELECT * FROM j_1bnfdoc  INTO TABLE nfdocs          "1175538
      IF NOT nf_number IS INITIAL.                          "1175538
        SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs     "1175538
                 WHERE nfnum  =  nf_number
                 AND   series =  series
                 AND   subser =  subseries
                 AND   parid  =  partner_id                 "1015050
                 AND   partyp =  partner_type               "1015050
                 AND   model  =  model
                 AND   doctyp <> '5'
                 AND   cancel =  space.

* when NFE is involved serie is set to 000                    "1244881
* check if normal NF with serie = space exists                "1244881
        IF NOT sy-subrc IS INITIAL                          "1244881
        AND NOT i_nfnum9 IS INITIAL                         "1244881
        AND series = '000'.                                 "1244881
          SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs   "1244881
                WHERE nfnum  =  nf_number                   "1244881
                AND   series =  space                       "1244881
                AND   subser =  subseries                   "1244881
                AND   parid  =  partner_id                  "1244881
                AND   partyp =  partner_type                "1244881
                AND   model  =  model
                AND   doctyp <> '5'                         "1244881
                AND   cancel =  space.                      "1244881
        ENDIF.                                              "1244881
*
        lv_subrc_nf = sy-subrc.                             "1175538
      ELSE.                                                 "1175538
        lv_subrc_nf = 4.                                    "1175538
      ENDIF.                                                "1175538
*      ELSE.                                                 "1175538
* for NF-e read with long NF number w/o subserie
*        SELECT * FROM j_1bnfdoc  INTO TABLE nfdocs          "1175538
      IF NOT i_nfnum9 IS INITIAL.                           "1175538
        SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs     "1175538
                 WHERE nfenum  = i_nfnum9
                 AND   series =  series
                 AND   parid  =  partner_id                 "1175538
                 AND   partyp =  partner_type               "1175538
                 AND   model  =  model
                 AND   doctyp <> '5'
                 AND   cancel =  space.
      ENDIF.

* at least one access was succesful                           "1175538
      IF sy-subrc IS INITIAL
      OR lv_subrc_nf IS INITIAL.                            "1175538

        CLEAR ref_found.
        LOOP AT nfdocs
         WHERE direct = '2'.
* check only outgoing documents for reference
          CLEAR   t_nflin.
          REFRESH t_nflin.

          SELECT * FROM j_1bnflin INTO TABLE t_nflin
           WHERE docnum = nfdocs-docnum.

          IF sy-subrc IS INITIAL.

            LOOP AT t_nflin
            WHERE reftyp = 'MD'.
              ref_found = 'X'.
              EXIT.
            ENDLOOP.

            IF NOT ref_found IS INITIAL.
* get the first one which was found
              doc_number = nfdocs-docnum.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.                         " loop at nfdocs

        IF ref_found IS INITIAL.
* reference document not found
          RAISE document_not_found.
        ENDIF.
      ELSE.
* reference document not found
        RAISE document_not_found.
      ENDIF.                             " if sy-subrc is initial.
*
*===========================================================
*--> Begin Note 774273

    ELSE.

***** select partner document

*     IF i_nfeflag IS INITIAL.                               "1175538
*       SELECT * FROM j_1bnfdoc INTO TABLE nfdocs            "1175538
      IF NOT nf_number IS INITIAL.                          "1175538
        SELECT * FROM j_1bnfdoc APPENDING TABLE nfdocs      "1175538
         WHERE     parid  =  partner_id
               AND partyp =  partner_type
               AND direct =  '1'
               AND nfnum  =  nf_number
               AND series =  series
               AND subser =  subseries
               AND model  =  model
               AND doctyp <> '5'
               AND cancel =  space.

* when NFE is involved serie is set to 000                    "1244881
* check if normal NF with serie = space exists                "1244881
        IF NOT sy-subrc IS INITIAL                          "1244881
        AND NOT i_nfnum9 IS INITIAL                         "1244881
        AND series = '000'.                                 "1244881
          SELECT * FROM j_1bnfdoc APPENDING TABLE nfdocs    "1244881
           WHERE     parid  =  partner_id                   "1244881
              AND partyp =  partner_type                    "1244881
              AND direct =  '1'                             "1244881
              AND nfnum  =  nf_number                       "1244881
              AND series =  space                           "1244881
              AND subser =  subseries                       "1244881
              AND model  =  model
              AND doctyp <> '5'                             "1244881
              AND cancel =  space.                          "1244881
        ENDIF.                                              "1244881

        lv_subrc_nf = sy-subrc.                             "1175538
      ELSE.                                                 "1175538
        lv_subrc_nf = 4.                                    "1175538
      ENDIF.                                                "1175538
*     ELSE.                                                  "1175538
* for NF-e read with long NF number w/o subserie
*        SELECT * FROM j_1bnfdoc  INTO TABLE nfdocs          "1175538
      IF NOT i_nfnum9 IS INITIAL.                           "1175538
        SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs     "1175538
         WHERE     parid  =  partner_id
               AND partyp =  partner_type
               AND direct =  '1'
               AND nfenum =  i_nfnum9
               AND series =  series
               AND model  =  model
               AND doctyp <> '5'
               AND cancel =  space.

      ENDIF.

* at least one access was succesful                          "1175538
      IF sy-subrc IS INITIAL
      OR lv_subrc_nf IS INITIAL.                            "1175538

*     If parameter DATE is filled, check if the date of the existing
*     NF's are different from the date of the NF that is to be created
        CLEAR lv_exist.

        LOOP AT nfdocs.
          IF nfdocs-docdat(4) = date(4).
*             Years are the same
            IF nfdocs-docdat = date.
*               Dates are the same
              doc_number = nfdocs-docnum.
              lv_exist = 1.
*               Exit loop because NF with same date and number was found
              EXIT.
            ELSE.
*               Years are the same, different dates:
              lv_exist = 2.
            ENDIF.
          ELSE.
*             Years differ
            IF lv_exist = 0.
*               Only set to 3 if not already 2 (stronger criterion)
              lv_exist = 3.
            ENDIF.
          ENDIF.
        ENDLOOP.

        CASE lv_exist.
          WHEN 1.
*             NF with same date and number was found
            CLEAR sy-subrc.
*             No exception means that NF was found.
            EXIT.
          WHEN 2.
*             Years are the same, different dates
            RAISE doc_with_same_year_found.
          WHEN 3.
*             Years differ
            RAISE doc_with_diff_year_found.
        ENDCASE.
      ELSE.
        RAISE document_not_found.
      ENDIF.
    ENDIF.
*--> End Note 774273
*===========================================================
*
  ELSEIF     company <> space
         AND branch  <> space.
***** select internal document
    IF NOT partner_id IS INITIAL.                    "note 562795

*     if i_nfeflag is initial.                                  "1175538

*       SELECT * FROM j_1bnfdoc INTO TABLE nfdocs   "note 562795"1175538
      IF NOT nf_number IS INITIAL.                          "1175538
        SELECT * FROM j_1bnfdoc APPENDING TABLE nfdocs      "1175538
       WHERE     bukrs  =  company                   "note 562795
             AND branch =  branch                    "note 562795
             AND nfnum  =  nf_number                 "note 562795
             AND series =  series                    "note 562795
             AND subser =  subseries                 "note 562795
             AND parid  =  partner_id                "note 562795
             AND model  =  model
             AND doctyp <> '5'                       "note 562795
             AND cancel =  space.                    "note 562795

* when NFE is involved serie is set to 000                    "1244881
* check if normal NF with serie = space exists                "1244881
        IF NOT sy-subrc IS INITIAL                          "1244881
        AND NOT i_nfnum9 IS INITIAL                         "1244881
        AND series = '000'.                                 "1244881
          SELECT * FROM j_1bnfdoc APPENDING TABLE nfdocs    "1244881
             WHERE     bukrs  =  company                    "1244881
             AND branch =  branch                           "1244881
             AND nfnum  =  nf_number                        "1244881
             AND series =  space                            "1244881
             AND subser =  subseries                        "1244881
             AND parid  =  partner_id                       "1244881
             AND model  =  model
             AND doctyp <> '5'                              "1244881
             AND cancel =  space.                           "1244881
        ENDIF.                                              "1244881

        lv_subrc_nf = sy-subrc.                             "1175538
      ELSE.                                                 "1175538
        lv_subrc_nf = 4.                                    "1175538
      ENDIF.                                                "1175538
*     else.
*NF-e
*       SELECT * FROM j_1bnfdoc  INTO TABLE nfdocs           "1175538
      IF NOT i_nfnum9 IS INITIAL.                           "1175538
        SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs     "1175538
         WHERE     bukrs  =  company
             AND branch =  branch
             AND nfenum =  i_nfnum9
             AND series =  series
             AND parid  =  partner_id
             AND model  =  model
             AND doctyp <> '5'
             AND cancel =  space.
      ENDIF.

    ELSE.                                             "note 562795

*     if i_nfeflag is initial.                                  "1175538

*       SELECT * FROM j_1bnfdoc INTO TABLE nfdocs               "1175538
      IF NOT nf_number IS INITIAL.                          "1175538
        SELECT * FROM j_1bnfdoc APPENDING TABLE nfdocs      "1175538
       WHERE     bukrs  =  company
             AND branch =  branch
             AND nfnum  =  nf_number
             AND series =  series
             AND subser =  subseries
             AND model  =  model
               AND doctyp <> '5'
               AND cancel =  space.

* when NFE is involved serie is set to 000                    "1244881
* check if normal NF with serie = space exists                "1244881
        IF NOT sy-subrc IS INITIAL                          "1244881
        AND NOT i_nfnum9 IS INITIAL                         "1244881
        AND series = '000'.                                 "1244881
          SELECT * FROM j_1bnfdoc APPENDING TABLE nfdocs    "1244881
           WHERE  bukrs  =  company                         "1244881
              AND branch =  branch                          "1244881
              AND nfnum  =  nf_number                       "1244881
              AND series =  space                           "1244881
              AND subser =  subseries                       "1244881
              AND model  =  model
              AND doctyp <> '5'                             "1244881
              AND cancel =  space.                          "1244881
        ENDIF.                                              "1244881

        lv_subrc_nf = sy-subrc.                             "1175538
      ELSE.                                                 "1175538
        lv_subrc_nf = 4.                                    "1175538
      ENDIF.                                                "1175538
*     else.
*NF-e
*       SELECT * FROM j_1bnfdoc  INTO TABLE nfdocs           "1175538
      IF NOT i_nfnum9 IS INITIAL.                           "1175538
        SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs     "1175538
         WHERE     bukrs  =  company
               AND branch =  branch
               AND nfenum =  i_nfnum9
               AND series =  series
               AND model  =  model
               AND doctyp <> '5'
               AND cancel =  space.

      ENDIF.

    ENDIF.                                           "note 562795

* at least one access was succesful                          "1175538
    IF sy-subrc IS INITIAL
    OR lv_subrc_nf IS INITIAL.                              "1175538

      READ TABLE nfdocs INDEX 1.
      IF sy-tfill = 1.
        doc_number = nfdocs-docnum.
      ELSE.
* checked entries shown popup allows maximum 25                 "1175538
        IF sy-tfill < 26.                                   "1175538
          CALL FUNCTION 'J_1B_DOCUMENT_SELECT_BY_USER'          "1175538
            IMPORTING                                           "1175538
              e_refdoc = doc_number                       "1175538
            TABLES                                              "1175538
              it_nfdoc = nfdocs                           "1175538
            .                                             "1175538
        ELSE.                                               "1175538
          who = company.
          MOVE branch TO who+4(4).
          CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
            EXPORTING
              nf_number  = nf_number
              series     = series
              subseries  = subseries
              nf_number9 = i_nfnum9
            IMPORTING
              ref_number = ref_number.
          MESSAGE e083 WITH ref_number who
          RAISING too_many_documents_found.
        ENDIF.                                              "1175538
      ENDIF.
    ELSE.
      RAISE document_not_found.
    ENDIF.
  ELSEIF    partner_id   <> space
        AND partner_type <> space.
***** select partner document

*     if i_nfeflag is initial.                                  "1175538

*       SELECT * FROM j_1bnfdoc INTO TABLE nfdocs               "1175538
    IF NOT nf_number IS INITIAL.                            "1175538
      SELECT * FROM j_1bnfdoc APPENDING TABLE nfdocs        "1175538
   WHERE     parid  =  partner_id
         AND partyp =  partner_type
         AND direct =  '1'
         AND nfnum  =  nf_number
         AND series =  series
         AND subser =  subseries
         AND model  =  model
           AND doctyp <> '5'
           AND cancel =  space.

* when NFE is involved serie is set to 000                    "1244881
* check if normal NF with serie = space exists                "1244881
      IF NOT sy-subrc IS INITIAL                            "1244881
      AND NOT i_nfnum9 IS INITIAL                           "1244881
      AND series = '000'.                                   "1244881
        SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs     "1244881
        WHERE parid  =  partner_id                          "1244881
          AND partyp =  partner_type                        "1244881
          AND direct =  '1'                                 "1244881
          AND nfnum  =  nf_number                           "1244881
          AND series =  space                               "1244881
          AND subser =  subseries                           "1244881
          AND model  =  model
          AND doctyp <> '5'                                 "1244881
          AND cancel =  space.                              "1244881
      ENDIF.                                                "1244881

      lv_subrc_nf = sy-subrc.                               "1175538
    ELSE.                                                   "1175538
      lv_subrc_nf = 4.                                      "1175538
    ENDIF.                                                  "1175538
*     else.

*       SELECT * FROM j_1bnfdoc  INTO TABLE nfdocs           "1175538
    IF NOT i_nfnum9 IS INITIAL.                             "1175538
      SELECT * FROM j_1bnfdoc  APPENDING TABLE nfdocs       "1175538
     WHERE     parid  =  partner_id
           AND partyp =  partner_type
           AND direct =  '1'
           AND nfenum =  i_nfnum9
           AND series =  series
           AND model  =  model
           AND doctyp <> '5'
           AND cancel =  space.

      DELETE nfdocs WHERE entrad EQ abap_true. "Descartar registros de entrada propria.
      IF nfdocs[] IS INITIAL.
        sy-subrc = 4.
      ELSE.
        sy-subrc = 0.
      ENDIF.

    ENDIF.

* at least one access was succesful                          "1175538
    IF sy-subrc IS INITIAL
    OR lv_subrc_nf IS INITIAL.                              "1175538

*     Note 550160:
*     If parameter DATE is filled, check if the date of the existing
*     NF's are different from the date of the NF that is to be created
      IF NOT date IS INITIAL.
        CLEAR lv_exist.

        LOOP AT nfdocs.
          IF nfdocs-docdat(4) = date(4).
*           Years are the same
            IF nfdocs-docdat = date.
*             Dates are the same
              doc_number = nfdocs-docnum.
              lv_exist = 1.
*             Exit loop because NF with same date and number was found
              EXIT.
            ELSE.
*             Years are the same, different dates:
              lv_exist = 2.
            ENDIF.
          ELSE.
*           Years differ
            IF lv_exist = 0.
*             Only set to 3 if not already 2 (stronger criterion)
              lv_exist = 3.
            ENDIF.
          ENDIF.
        ENDLOOP.

        CASE lv_exist.
          WHEN 1.
*           NF with same date and number was found
            CLEAR sy-subrc.
*           No exception means that NF was found.
            EXIT.
          WHEN 2.
*           Years are the same, different dates
            RAISE doc_with_same_year_found.
          WHEN 3.
*           Years differ
            RAISE doc_with_diff_year_found.
        ENDCASE.

      ENDIF.

*     This is only executed if parameter DATE is not supplied
*     (previous behaviour before Note 550160):
      READ TABLE nfdocs INDEX 1.
      IF sy-tfill = 1.
        doc_number = nfdocs-docnum.
      ELSE.
* checked entries shown popup allows maximum 25                 "1175538
        IF sy-tfill < 26.                                   "1175538
          CALL FUNCTION 'J_1B_DOCUMENT_SELECT_BY_USER'          "1175538
            IMPORTING                                           "1175538
              e_refdoc = doc_number                       "1175538
            TABLES                                              "1175538
              it_nfdoc = nfdocs                           "1175538
            .                                             "1175538
        ELSE.                                               "1175538
          CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
            EXPORTING
              nf_number  = nf_number
              series     = series
              subseries  = subseries
              nf_number9 = i_nfnum9
            IMPORTING
              ref_number = ref_number.
          MESSAGE e082 WITH ref_number partner_id
          RAISING too_many_documents_found.
        ENDIF.                                              "1175538
      ENDIF.
    ELSE.
      RAISE document_not_found.
    ENDIF.
  ELSE.
    RAISE document_not_found.
  ENDIF.

ENDFUNCTION.
