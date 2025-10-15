"Name: \PR:SAPLV61A\FO:XKOMV_KWERT_ERMITTELN\SE:BEGIN\EI
ENHANCEMENT 0 Z_CALCULO_VF01.
*
   " Antonio Luiz 29.11.2012
   DATA:
     vl_tabix_xkomv TYPE sy-tabix,
     vl_vbelv       TYPE vbfa-vbelv,
     vl_fkart       TYPE vbrk-fkart,
     vg_matnr       TYPE vbap-matnr,
     vl_kwert       TYPE komv-kbetr,
     vg_kbetr       TYPE kbetr,
*--> 23/05/2023 - Migração S4 – PS
*         VG_VAKEY        TYPE KONH-VAKEY,
     vg_vakey       TYPE vakey_long,
     vg_vakey_migr  TYPE vakey_long,
     vg_idx_migr    TYPE sy-tabix,
*<-- 23/05/2023 - Migração S4 – PS
     vg_knumv       TYPE vbak-knumv,
     vg_taxa        TYPE konv-kbetr,
     wa_vbap        TYPE vbap,
     wa_lfa1        TYPE lfa1,
     vg_valor(16)   TYPE p DECIMALS 5,
     vlifnr         TYPE lfa1-lifnr.

   DATA: vg_kbetr_aux TYPE kbetr,
         wa_setleaf   TYPE setleaf,
         vperc(12)    TYPE p DECIMALS 6.

   DATA: xkomv_aux   TYPE STANDARD TABLE OF komv_index WITH HEADER LINE,
         it_konh     TYPE TABLE OF konh WITH HEADER LINE,
         wa_konh     TYPE konh,
         wa_konh_ant TYPE konh.

*-#133089-21.02.2024-JT-inicio
   DATA: t_callstack  TYPE abap_callstack,
         lc_fat_autom TYPE char01.
   FIELD-SYMBOLS: <f_fatauto> TYPE any.

   CALL FUNCTION 'SYSTEM_CALLSTACK'
     EXPORTING
       max_level = 0
     IMPORTING
       callstack = t_callstack.

*------------------
* faturamento automatico
*------------------
   READ TABLE t_callstack INTO DATA(w_callstack) WITH KEY mainprogram = 'ZLESR0180_JOB'.
   IF sy-subrc = 0.
     lc_fat_autom = abap_true.
   ENDIF.

   IF lc_fat_autom = abap_false.
     ASSIGN ('(ZLESR0013)p_fataut') TO <f_fatauto>.
     IF sy-subrc = 0 AND <f_fatauto> = abap_true.
       lc_fat_autom = abap_true.
     ENDIF.
   ENDIF.
*-#133089-21.02.2024-JT-fim

*--> 23/05/2023 - Migração S4 – PS
   DATA:
     lo_cond_vakey_srv TYPE REF TO cl_cond_vakey_srv,
     o_cx_ref          TYPE REF TO cx_cond_vakey.
*<-- 23/05/2023 - Migração S4 – PS

*     IF ( SY-TCODE+0(2) = 'VA') or ( SY-TCODE+0(2) = 'VF').
*         IF XKOMV-KSCHL = 'ZFUN'.
*            XKOMV_AUX[] = XKOMV[].
*            READ TABLE XKOMV_AUX WITH KEY KSCHL = 'IBRX' .
*            if  XKOMV_AUX-KAWRT gt 0.
*               XKOMV-KWERT = ( XKOMV_AUX-KAWRT * XKOMV-KBETR ) / 10000 .
*               xkomv-kdupl = 'B'.
*            Endif.
*         Elseif XKOMV-KSCHL = 'ZSEN'.
*           XKOMV-KWERT = ( ykmeng * XKOMV-KBETR ) / 100000.
*           xkomv-kdupl = 'B'.
*         Endif.
*         "
*     Endif.

   FIELD-SYMBOLS: <fs_likp> TYPE likp.
   FIELD-SYMBOLS: <vg_likp> TYPE likp.
   IF xkomv-kschl = 'ICMI' .
     xkomv_aux[] = xkomv[].
     READ TABLE xkomv_aux WITH KEY kschl = 'PR00' .
     vl_tabix_xkomv = sy-tabix.
     IF ( sy-tcode = 'VF01' OR sy-tcode = 'ZLES0136' OR sy-tcode = 'ZMM0127' OR lc_fat_autom = abap_true ) AND ( sy-subrc IS INITIAL ). "*-#133089-21.02.2024-JT
       "Busca Valor de Nova Pauta ----------------------------------------------------------------------------
       "------------------------------------------------------------------------------------------------------
       ASSIGN ('(SAPLV60A)LIKP') TO <vg_likp>.

       IF <vg_likp> IS ASSIGNED.
         CLEAR vl_fkart.
         vl_fkart = <vg_likp>-fkarv.
         IF  ( vl_fkart  EQ 'ZRFL' ) OR ( vl_fkart EQ 'ZRDC' ) OR ( vl_fkart EQ 'ZIND' ).
           CLEAR vl_vbelv.
           SELECT vbelv INTO vl_vbelv
                        FROM vbfa
                    UP TO 1 ROWS
                       WHERE vbeln = <vg_likp>-vbeln
                         AND vbtyp_n = 'J'
                         AND vbtyp_v = 'C'.
           ENDSELECT.

           IF NOT vl_vbelv IS INITIAL.
             SELECT SINGLE *
               FROM vbap
               INTO wa_vbap
              WHERE vbeln = vl_vbelv.

             CLEAR vlifnr.
             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = wa_vbap-werks
               IMPORTING
                 output = vlifnr.

             SELECT SINGLE *
               FROM setleaf
               INTO wa_setleaf
              WHERE setname = 'MAGGI_MATKL_VF01'
                AND valfrom = wa_vbap-matkl.

             SELECT SINGLE *
               FROM lfa1
               INTO wa_lfa1
              WHERE lifnr EQ vlifnr.

             vg_taxa = 100.
             IF  vl_fkart EQ 'ZIND'.
               SELECT SINGLE knumv
                 FROM vbak
                 INTO vg_knumv
                 WHERE vbeln = vl_vbelv.
               "
*                  TRY.

               cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
                 EXPORTING it_selection_attribute = VALUE #(
                ( fieldname = 'KNUMV' value = vg_knumv )
                ( fieldname = 'KSCHL' value = 'BX13' )
                )
                 IMPORTING et_prc_element_classic_format = DATA(etl109c18r5752) ).
               vg_taxa = etl109c18r5752[ 1 ]-kbetr.
*CATCH CX_PRC_RESULT CX_SY_ITAB_LINE_NOT_FOUND .
*  SY-SUBRC = 4.
*ENDTRY.
               vg_taxa = 100000 - vg_taxa.
               vg_taxa = vg_taxa / 1000.
             ENDIF.

             IF ( sy-subrc IS INITIAL ) AND wa_vbap-matnr IS NOT INITIAL.
               CONCATENATE  <vg_likp>-inco1 wa_vbap-matnr INTO vg_vakey.
               CONCATENATE wa_lfa1-land1 wa_lfa1-regio vg_vakey INTO vg_vakey SEPARATED BY space.

               SELECT SINGLE konh~mandt
                             konh~knumh
                             konh~ernam
                             konh~erdat
                             konh~kvewe
                             konh~kotabnr
                             konh~kappl
                             konh~kschl
*                                KONH~VAKEY
                             konh~datab
                             konh~datbi
                             konh~kosrt
                             konh~kzust
                             konh~knuma_pi
                             konh~knuma_ag
                             konh~knuma_sq
                             konh~knuma_sd
                             konh~aktnr
                             konh~knuma_bo
                             konh~licno
                             konh~licdt
*                                KONH~VADAT
                 INTO it_konh
                 FROM konh
                 INNER JOIN konp ON  konp~knumh EQ konh~knumh
                 AND   konp~loevm_ko EQ ''
                 WHERE konh~kvewe = 'A'
                 AND konh~kotabnr = 924
                 AND konh~kschl   = 'ZIVP'
*                    AND KONH~VAKEY   = VG_VAKEY
                 AND konh~datab   LE sy-datum
                 AND konh~datbi   GE sy-datum.

*--> 23/05/2023 - Migração S4 – PS
               LOOP AT it_konh ASSIGNING FIELD-SYMBOL(<fs_konh_migr>).

                 CLEAR: vg_vakey_migr, vg_idx_migr.

                 vg_idx_migr = sy-tabix.

                 lo_cond_vakey_srv = cl_cond_vakey_srv=>get_instance( ).
*                     TRY.
                 vg_vakey_migr = lo_cond_vakey_srv->determine_vakey_from_db(
                                                                        iv_usage   = <fs_konh_migr>-kvewe
                                                                        iv_knumh   = <fs_konh_migr>-knumh
                                                                        iv_kotabnr = <fs_konh_migr>-kotabnr
                                                                        iv_bypassing_buffer = abap_false ).
*                     cATCH cx_cond_vakey INTO o_cx_ref.
*
*                       delete IT_KONH INDEX vg_idx_migr.
*                       continue.
*
*                     ENDTRY.

                 IF vg_vakey_migr NE vg_vakey.

                   DELETE it_konh INDEX vg_idx_migr.
                   CONTINUE.

                 ENDIF.

               ENDLOOP.
*<-- 23/05/2023 - Migração S4 – PS

*--> 23/05/2023 - Migração S4 – PS
*                   if sy-subrc ne 0. " Se não encontrar na região do fornecedor localizar por 'MT' Liam
               IF it_konh[] IS INITIAL. " Se não encontrar na região do fornecedor localizar por 'MT' Liam
*<-- 23/05/2023 - Migração S4 – PS
                 CLEAR vg_vakey.
                 CONCATENATE  <vg_likp>-inco1 wa_vbap-matnr INTO vg_vakey.
                 CONCATENATE wa_lfa1-land1 'MT' vg_vakey INTO vg_vakey SEPARATED BY space.
*                      SELECT SINGLE * INTO IT_KONH
*                      FROM KONH
*                       WHERE KVEWE = 'A'
*                       AND KOTABNR = 924
*                       AND KSCHL   = 'ZIVP'
*                       AND VAKEY   = VG_VAKEY.
                 SELECT SINGLE konh~mandt
                          konh~knumh
                          konh~ernam
                          konh~erdat
                          konh~kvewe
                          konh~kotabnr
                          konh~kappl
                          konh~kschl
*--> 23/05/2023 - Migração S4 – PS
*                                KONH~VAKEY
*<-- 23/05/2023 - Migração S4 – PS
                          konh~datab
                          konh~datbi
                          konh~kosrt
                          konh~kzust
                          konh~knuma_pi
                          konh~knuma_ag
                          konh~knuma_sq
                          konh~knuma_sd
                          konh~aktnr
                          konh~knuma_bo
                          konh~licno
                          konh~licdt
*--> 23/05/2023 - Migração S4 – PS
*                                KONH~VADAT INTO IT_KONH
                          INTO CORRESPONDING FIELDS OF it_konh
*<-- 23/05/2023 - Migração S4 – PS
                   FROM konh
                   INNER JOIN konp ON  konp~knumh EQ konh~knumh
                   AND   konp~loevm_ko EQ ''
                   WHERE konh~kvewe = 'A'
                   AND konh~kotabnr = 924
                   AND konh~kschl   = 'ZIVP'
*                         AND KONH~VAKEY   = VG_VAKEY
                   AND konh~datab   LE sy-datum
                   AND konh~datbi   GE sy-datum.

*--> 23/05/2023 - Migração S4 – PS
                 LOOP AT it_konh ASSIGNING <fs_konh_migr>.

                   CLEAR: vg_vakey_migr, vg_idx_migr.

                   vg_idx_migr = sy-tabix.

                   lo_cond_vakey_srv = cl_cond_vakey_srv=>get_instance( ).
*                          TRY.
                   vg_vakey_migr = lo_cond_vakey_srv->determine_vakey_from_db(
                                                                          iv_usage   = <fs_konh_migr>-kvewe
                                                                          iv_knumh   = <fs_konh_migr>-knumh
                                                                          iv_kotabnr = <fs_konh_migr>-kotabnr
                                                                          iv_bypassing_buffer = abap_false ).
*                          cATCH cx_cond_vakey INTO o_cx_ref.
*
*                            delete IT_KONH INDEX vg_idx_migr.
*                            continue.
*
*                          ENDTRY.

                   IF vg_vakey_migr NE vg_vakey.

                     DELETE it_konh INDEX vg_idx_migr.
                     CONTINUE.

                   ENDIF.

                 ENDLOOP.
*<-- 23/05/2023 - Migração S4 – PS

               ENDIF.

*--> 23/05/2023 - Migração S4 – PS
*                  IF SY-SUBRC IS INITIAL.
               IF it_konh[] IS NOT INITIAL.
*<-- 23/05/2023 - Migração S4 – PS
*                    SELECT * INTO TABLE IT_KONH
*                      FROM KONH
*                     WHERE KVEWE   EQ IT_KONH-KVEWE
*                       AND KOTABNR EQ IT_KONH-KOTABNR
*                       AND KAPPL   EQ IT_KONH-KAPPL
*                       AND KSCHL   EQ IT_KONH-KSCHL
*                       AND VAKEY   EQ IT_KONH-VAKEY .
                 SELECT   konh~mandt
                          konh~knumh
                          konh~ernam
                          konh~erdat
                          konh~kvewe
                          konh~kotabnr
                          konh~kappl
                          konh~kschl
*--> 23/05/2023 - Migração S4 – PS
*                             KONH~VAKEY
*<-- 23/05/2023 - Migração S4 – PS
                          konh~datab
                          konh~datbi
                          konh~kosrt
                          konh~kzust
                          konh~knuma_pi
                          konh~knuma_ag
                          konh~knuma_sq
                          konh~knuma_sd
                          konh~aktnr
                          konh~knuma_bo
                          konh~licno
                          konh~licdt
*--> 23/05/2023 - Migração S4 – PS
*                             KONH~VADAT INTO TABLE IT_KONH
                          INTO CORRESPONDING FIELDS OF TABLE it_konh
*<-- 23/05/2023 - Migração S4 – PS
                   FROM konh
                   INNER JOIN konp ON  konp~knumh EQ konh~knumh
                    AND   konp~loevm_ko EQ ''
                  WHERE konh~kvewe   EQ it_konh-kvewe
                    AND konh~kotabnr EQ it_konh-kotabnr
                    AND konh~kappl   EQ it_konh-kappl
                    AND konh~kschl   EQ it_konh-kschl
*                       AND KONH~VAKEY   EQ IT_KONH-VAKEY
                    AND konh~datab   LE sy-datum
                    AND konh~datbi   GE sy-datum.

*--> 23/05/2023 - Migração S4 – PS
                 LOOP AT it_konh ASSIGNING <fs_konh_migr>.

                   CLEAR: vg_vakey_migr, vg_idx_migr.

                   vg_idx_migr = sy-tabix.

                   lo_cond_vakey_srv = cl_cond_vakey_srv=>get_instance( ).
*                      TRY.
                   vg_vakey_migr = lo_cond_vakey_srv->determine_vakey_from_db(
                                                                          iv_usage   = <fs_konh_migr>-kvewe
                                                                          iv_knumh   = <fs_konh_migr>-knumh
                                                                          iv_kotabnr = <fs_konh_migr>-kotabnr
                                                                          iv_bypassing_buffer = abap_false ).
*                      cATCH cx_cond_vakey INTO o_cx_ref.
*
*                        delete IT_KONH INDEX vg_idx_migr.
*                        continue.
*
*                      ENDTRY.

                   IF vg_vakey_migr NE vg_vakey.

                     DELETE it_konh INDEX vg_idx_migr.
                     CONTINUE.

                   ENDIF.

                 ENDLOOP.
*<-- 23/05/2023 - Migração S4 – PS

                 CLEAR wa_konh_ant.
                 SORT it_konh BY datab.
                 LOOP AT it_konh.
                   IF it_konh-datab GT sy-datum OR
                      it_konh-datbi LT sy-datum.
                     EXIT.
                   ENDIF.
                   MOVE-CORRESPONDING it_konh TO wa_konh_ant.
                 ENDLOOP.
                 MOVE-CORRESPONDING wa_konh_ant TO wa_konh.

                 IF NOT wa_konh-knumh IS INITIAL.

                   IF NOT ( wa_setleaf IS INITIAL ).

                     SELECT SINGLE kbetr INTO vg_kbetr_aux
                       FROM konp
                      WHERE knumh EQ wa_konh-knumh
                      AND   loevm_ko EQ ''.

                     IF ( vg_kbetr_aux > xkomv_aux-kbetr ).
                       xkomv_aux-kbetr = vg_kbetr_aux.
                     ENDIF.


                   ELSE.
                     SELECT SINGLE kbetr INTO xkomv_aux-kbetr
                       FROM konp
                      WHERE knumh EQ wa_konh-knumh
                      AND   loevm_ko EQ ''.
                   ENDIF.


                   IF sy-subrc IS INITIAL.
                     IF vl_fkart EQ 'ZIND'.
                       IF vg_taxa EQ 100.
                         MESSAGE e000(z01)
                                 WITH 'Taxa de redução para remessa ind. com erro'
                                      'Contacte a T.I.'.
                       ENDIF.
                       vg_valor = ( xkomv_aux-kbetr * vg_taxa ) * 10.
                       xkomv-kbetr = vg_valor / 1000.
                       xkomv-kwert = ( vg_valor * ( <vg_likp>-btgew / 1000 ) ) / 1000.
                       komp-netwr = xkomv-kwert.
                     ELSE.
                       xkomv-kbetr = xkomv_aux-kbetr.
                       xkomv-kwert =  xkomv_aux-kbetr * ( <vg_likp>-btgew / 1000 ) .
                     ENDIF.
                     MODIFY xkomv INDEX vl_tabix_xkomv TRANSPORTING kbetr kwert.
                   ENDIF.
                 ENDIF.
               ENDIF.
             ENDIF.
           ENDIF.
         ENDIF.
       ENDIF.
       "------------------------------------------------------------------------------------------------------
       "------------------------------------------------------------------------------------------------------
     ENDIF.
   ENDIF.
ENDENHANCEMENT.
