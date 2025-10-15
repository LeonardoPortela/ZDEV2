"Name: \PR:SAPLMR1M\FO:VARIANT_TRANSACTION\SE:BEGIN\EI
ENHANCEMENT 0 Z_MIRO_NFE.
*
   DATA: wa_bkpf_fat TYPE bkpf,
         wa_bkpf_est TYPE bkpf,
         p_mode      LIKE rfpdo-allgazmd.

   DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
         l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
         l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately

   DATA: lt_blntab  TYPE STANDARD TABLE OF blntab  WITH HEADER LINE,
         lt_ftclear TYPE STANDARD TABLE OF ftclear WITH HEADER LINE,
         lt_ftpost  TYPE STANDARD TABLE OF ftpost  WITH HEADER LINE,
         lt_fttax   TYPE STANDARD TABLE OF fttax   WITH HEADER LINE,
         lds_return TYPE bapiret2.

   " busca texto do item do documento contabil
   DATA: i_bseg TYPE bseg OCCURS 1,
         i_bkpf TYPE bkpf OCCURS 1,
         i_bsec TYPE bsec OCCURS 1,       " int. table for bsec
         i_bsed TYPE bsed OCCURS 1,       " int. table for bsed
         i_bset TYPE bset OCCURS 1,       " int. table for bset
         i_bkdf TYPE bkdf OCCURS 1.       " int. table for bkdf

   IF sy-tcode = 'MR8M'. " or ( sy-tcode = 'MIRO' and sy-UCOMM = 'OK' and sy-dynnr = '6000' ).
     WAIT UP TO 3 SECONDS.
     CONCATENATE rbkpv-belnr rbkpv-gjahr INTO wa_bkpf_fat-awkey.
     CONCATENATE st_belnr st_gjahr INTO wa_bkpf_est-awkey.
     "
     SELECT SINGLE * FROM bkpf INTO @DATA(wa_bkpf_ori)
            WHERE awkey = @wa_bkpf_fat-awkey
            AND   blart = 'ZG'.
     IF sy-subrc = 0.
* ---> S4 Migration - 19/06/2023 - MA
*      SELECT SINGLE * FROM BSEG INTO @DATA(WA_BSEG_ORI)
*          WHERE BUKRS = @WA_BKPF_ORI-BUKRS AND
*                BELNR = @WA_BKPF_ORI-BELNR AND
*                GJAHR = @WA_BKPF_ORI-GJAHR AND
*                BSCHL = '31'.

       DATA: lt_bseg     TYPE fagl_t_bseg,
             wa_bseg_ori TYPE bseg.

       CALL FUNCTION 'FAGL_GET_BSEG'
         EXPORTING
           i_bukrs   = wa_bkpf_ori-bukrs
           i_belnr   = wa_bkpf_ori-belnr
           i_gjahr   = wa_bkpf_ori-gjahr
         IMPORTING
           et_bseg   = lt_bseg
         EXCEPTIONS
           not_found = 1
           OTHERS    = 2.

       DELETE lt_bseg WHERE bschl NE '31'.

       READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
       IF sy-subrc = 0.
         MOVE-CORRESPONDING ls_bseg TO wa_bseg_ori.
       ENDIF.
*<--- S4 Migration - 19/06/2023 - MA

       IF wa_bseg_ori-sgtxt IS NOT INITIAL.
         SELECT SINGLE * FROM bkpf INTO wa_bkpf_est
           WHERE awkey = wa_bkpf_est-awkey
           AND   blart = 'ZG'.
         IF sy-subrc = 0.
           REFRESH: i_bkpf, i_bseg.
           APPEND wa_bkpf_est TO i_bkpf.
           DATA rldnr_l46c10r1877 TYPE rldnr.
           CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
             IMPORTING
               e_rldnr       = rldnr_l46c10r1877
             EXCEPTIONS
               not_found     = 1
               more_than_one = 2.
           IF sy-subrc = 0.
             CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
               EXPORTING
                 i_rldnr         = rldnr_l46c10r1877
                 i_bukrs         = wa_bkpf_est-bukrs
                 i_belnr         = wa_bkpf_est-belnr
                 i_gjahr         = wa_bkpf_est-gjahr
                 it_where_clause = VALUE tt_rsdswhere( ( |BSCHL = '21'| ) )
               IMPORTING
                 et_bseg         = i_bseg
               EXCEPTIONS
                 not_found       = 1.
           ENDIF.
           IF sy-subrc <> 0 OR lines( i_bseg ) = 0.
             sy-subrc = 4.
             sy-dbcnt = 0.
           ELSE.
             sy-dbcnt = lines( i_bseg ).
           ENDIF.


           IF wa_bseg_ori-sgtxt IS NOT INITIAL.
             LOOP AT i_bseg INTO DATA(wa_bseg).
               wa_bseg-sgtxt = wa_bseg_ori-sgtxt.
               MODIFY i_bseg FROM wa_bseg.
             ENDLOOP.
           ENDIF.

           CALL FUNCTION 'CHANGE_DOCUMENT'
             TABLES
               t_bkdf = i_bkdf
               t_bkpf = i_bkpf
               t_bsec = i_bsec
               t_bsed = i_bsed
               t_bseg = i_bseg
               t_bset = i_bset.
           IF sy-subrc = 0.
             WAIT UP TO 1 SECONDS.
             CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
               EXPORTING
                 wait = 'X'.
             WAIT UP TO 1 SECONDS.
           ENDIF.
         ENDIF.
       ENDIF.
     ENDIF.
     "ALRS
     CONCATENATE rbkpv-belnr rbkpv-gjahr INTO wa_bkpf_fat-awkey.
     SELECT SINGLE *
       FROM bkpf
       INTO wa_bkpf_fat
       WHERE awkey = wa_bkpf_fat-awkey.
     "
     CONCATENATE st_belnr st_gjahr INTO wa_bkpf_est-awkey.
     SELECT SINGLE *
       FROM bkpf
       INTO wa_bkpf_est
       WHERE awkey = wa_bkpf_est-awkey.

     IF sy-subrc = 0.
       p_mode = 'N'.
       CALL FUNCTION 'POSTING_INTERFACE_START'
         EXPORTING
           i_client           = sy-mandt
           i_function         = 'C'
           i_mode             = p_mode
           i_update           = 'S'
           i_user             = sy-uname
         EXCEPTIONS
           client_incorrect   = 1
           function_invalid   = 2
           group_name_missing = 3
           mode_invalid       = 4
           update_invalid     = 5
           OTHERS             = 6.

       IF sy-subrc <> 0.
         EXIT.
       ENDIF.

       lt_ftpost-stype = 'K'."Header
       lt_ftpost-count = 1.  "number of Dynpro

       lt_ftpost-fnam = 'BKPF-BUKRS'.
       lt_ftpost-fval =  wa_bkpf_fat-bukrs.
       APPEND lt_ftpost.

       lt_ftpost-fnam = 'BKPF-WAERS'.
       lt_ftpost-fval = wa_bkpf_fat-waers.
       APPEND lt_ftpost.

       lt_ftpost-fnam = 'BKPF-BLDAT'.
       CONCATENATE wa_bkpf_est-budat+6(2) wa_bkpf_est-budat+4(2) wa_bkpf_est-budat(4) INTO lt_ftpost-fval SEPARATED BY '.'.
       APPEND lt_ftpost.

       lt_ftpost-fnam = 'BKPF-BUDAT'.
       CONCATENATE wa_bkpf_est-budat+6(2) wa_bkpf_est-budat+4(2) wa_bkpf_est-budat(4) INTO lt_ftpost-fval SEPARATED BY '.'.
       APPEND lt_ftpost.

       lt_ftpost-fnam = 'BKPF-MONAT'.
       lt_ftpost-fval =  wa_bkpf_est-budat+4(2).
       APPEND lt_ftpost.

       lt_ftpost-fnam = 'BKPF-BLART'.
       lt_ftpost-fval = wa_bkpf_est-blart. "'RE'. "CS2017000118
       APPEND lt_ftpost.

       lt_ftpost-fnam = 'BKPF-XBLNR'.
       lt_ftpost-fval = 'EST.MR8M' .
       APPEND lt_ftpost.

       lt_ftclear-agkoa  = 'K'.
       lt_ftclear-agkon  = rbkpv-lifnr.
       lt_ftclear-agbuk  = rbkpv-bukrs.
       lt_ftclear-xnops  = 'X'.
       lt_ftclear-selfd  = 'BELNR'.
       lt_ftclear-selvon = wa_bkpf_fat-belnr.
       APPEND lt_ftclear.

       lt_ftclear-agkoa  = 'K'.
       lt_ftclear-agkon  = rbkpv-lifnr.
       lt_ftclear-agbuk  = rbkpv-bukrs.
       lt_ftclear-xnops  = 'X'.
       lt_ftclear-selfd  = 'BELNR'.
       lt_ftclear-selvon = wa_bkpf_EST-belnr.
       APPEND lt_ftclear.

       CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
         EXPORTING
           i_auglv                    = l_auglv
           i_tcode                    = l_tcode
           i_sgfunct                  = l_sgfunct
           i_no_auth                  = 'X'
         IMPORTING
           e_msgid                    = lds_return-id
           e_msgno                    = lds_return-number
           e_msgty                    = lds_return-type
           e_msgv1                    = lds_return-message_v1
           e_msgv2                    = lds_return-message_v2
           e_msgv3                    = lds_return-message_v3
           e_msgv4                    = lds_return-message_v4
         TABLES
           t_blntab                   = lt_blntab
           t_ftclear                  = lt_ftclear
           t_ftpost                   = lt_ftpost
           t_fttax                    = lt_fttax
         EXCEPTIONS
           clearing_procedure_invalid = 1
           clearing_procedure_missing = 2
           table_t041a_empty          = 3
           transaction_code_invalid   = 4
           amount_format_error        = 5
           too_many_line_items        = 6
           company_code_invalid       = 7
           screen_not_found           = 8
           no_authorization           = 9
           OTHERS                     = 10.

       CALL FUNCTION 'POSTING_INTERFACE_END'
         EXPORTING
           i_bdcimmed              = 'X'
         EXCEPTIONS
           session_not_processable = 1
           OTHERS                  = 2.

       IF sy-subrc <> 0.
         EXIT.
*             RETURN.
       ENDIF.
     ENDIF.

   ENDIF.

   IF sy-tcode = 'MIRO'.
     FIELD-SYMBOLS: <fs_docnum>     TYPE any.

     DATA: wa_zib_nfe_forn   TYPE zib_nfe_forn,
           t_zib_nfe_forn    TYPE TABLE OF zib_nfe_forn WITH HEADER LINE,
           v_docnum(40),
           P_docnum          TYPE j_1bnfe_active-docnum,
           wa_J_1BNFE_ACTIVE TYPE j_1bnfe_active,
           wa_J_1BNFDOC      TYPE j_1bnfdoc.


     v_docnum = '(SAPLJ1BI)NFDOCNUM'.
     ASSIGN (v_docnum) TO <fs_docnum>.

     IF  <fs_docnum> IS ASSIGNED.
       MOVE <fs_docnum> TO p_docnum.
       IF p_docnum IS NOT INITIAL.
         WAIT UP TO 2 SECONDS.
         SELECT SINGLE *
           FROM j_1bnfe_active
           INTO wa_j_1bnfe_active
           WHERE docnum = P_docnum.

         SELECT SINGLE *
           FROM j_1bnfdoc
           INTO wa_j_1bnfdoc
           WHERE docnum = P_docnum.

         "Busca documento eletrônico selecionado
         SELECT SINGLE *  INTO wa_zib_nfe_forn
           FROM zib_nfe_forn
          WHERE nu_chave_cnpj   EQ wa_j_1bnfe_active-stcd1
            AND nu_chave_numero EQ wa_j_1bnfe_active-nfnum9
            AND nu_chave_serie  EQ wa_j_1bnfe_active-serie
            AND nu_chave_modelo EQ wa_j_1bnfe_active-model
            AND dt_emissao      EQ wa_j_1bnfdoc-docdat
            AND branch          EQ wa_j_1bnfe_active-branch.

         IF sy-subrc = 0.
           APPEND wa_zib_nfe_forn TO t_zib_nfe_forn.
           CALL FUNCTION 'Z_INFO_NFE_FORNECEDOR'
             TABLES
               it_info_notas = t_zib_nfe_forn.
         ENDIF.
       ENDIF.
     ENDIF.


   ENDIF.
ENDENHANCEMENT.
