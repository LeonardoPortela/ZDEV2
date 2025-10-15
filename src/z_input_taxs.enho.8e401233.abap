"Name: \PR:SAPLJ1BG\FO:NF_OBJECT_ADD\SE:BEGIN\EI
ENHANCEMENT 0 Z_INPUT_TAXS.
*Insere linhas de IPSN e ICOF na tabela de impostos da NF
  DATA: tl_0035      TYPE TABLE OF zsdt0035 WITH HEADER LINE,
        tl_setleaf   TYPE TABLE OF setleaf WITH HEADER LINE,
        vg_auart     TYPE auart,
        vg_j_1btxsdc TYPE j_1btxsdc_.

  TYPES: BEGIN OF ty_konv_ztrt,
           krech   TYPE konv-krech,
           kbetr   TYPE konv-kbetr,
           knumv   TYPE konv-knumv,
           kschl   TYPE konv-kschl,
           kawrt   TYPE konv-kawrt,
           kwert   TYPE konv-kwert,
           kbetr_n TYPE konv-kbetr,
         END OF ty_konv_ztrt.

  DATA:
    wa_tx        TYPE j_1bnfstx,
    it_konv_ztrt TYPE TABLE OF ty_konv_ztrt,
    wa_konv_ztrt TYPE ty_konv_ztrt,

    vl_tax_val   TYPE j_1bnfstx-taxval.

  CLEAR: tl_setleaf, vg_auart, wa_tx.
  REFRESH: tl_setleaf.

*-#143658-12.07.2024-JT-inicio
*****************************
* faturamento automatico
*****************************
  DATA: lc_ch_referencia  TYPE zch_ref.
  IMPORT lc_ch_referencia FROM MEMORY ID 'FAT_AUTOMATICO'.
*-#143658-12.07.2024-JT-fim

  """"" HAna
  IF wvbrp-vgbel NE wvbrp-aubel.
    wtxtnam(10) = wvbrp-vgbel.
    wtxtnam+10(6) = wvbrp-vgpos.
    SELECT SINGLE * FROM  lips
           WHERE  vbeln       = wvbrp-vgbel
           AND    posnr       = wvbrp-vgpos.
    IF sy-subrc = 0.
      IF lips-vgtyp = 'E'.
        wvgtyp = lips-vgtyp.
      ENDIF.
    ENDIF.
  ELSE.
    wtxtnam(10) = wvbrp-aubel.
    wtxtnam+10(6) = wvbrp-aupos.
  ENDIF.

  IF ( sy-tcode EQ 'VF01' OR sy-tcode EQ 'ZLES0077' ) AND 'ZROB' CS wvbrk-fkart .
    LOOP AT wnfstx.
      IF wnfstx-taxtyp = 'ICM3'.
        wnfstx-base = wnfstx-taxval / ( wnfstx-rate / 100 ).
        MODIFY wnfstx. "Modifica a tabela dos Impostos
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wvbrk-fkart EQ 'ZFUT'.
    LOOP AT wnfstx.
      IF wnfstx-taxtyp = 'ICM3'.
        wnfstx-stattx = space.
        MODIFY wnfstx. "Modifica a tabela dos Impostos
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF ( sy-tcode EQ 'VF01' OR
       sy-tcode EQ 'ZLES0077' OR
       sy-tcode EQ 'ZLES0136' OR
       sy-tcode EQ 'ZSDT0087' OR
       sy-tcode EQ 'ZSDT0081' OR  "// US-169490 WBARBOSA 24/07/2025
       sy-tcode EQ 'ZMM0127' OR
       sy-tcode EQ 'ZNFE' OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
       lc_ch_referencia   IS NOT INITIAL  ). "*-#143658-12.07.2024-JT
    IF wvbrk-fkart = 'ZIND'.
      DELETE wnfstx WHERE taxtyp  = 'ICM0'.
      DELETE wnfstx WHERE taxtyp  = 'ICMO'.
    ENDIF.
    "
    SELECT  *
      FROM j_1bnftxcond
      INTO TABLE @DATA(_tabela)
      WHERE kalsm   EQ @wvbrk-kalsm
      AND   taxtyp  IN ( 'ICM0', 'ICMO' ).

    IF lines( _tabela ) = 2.
      DELETE wnfstx WHERE taxtyp  = 'ICM0'.
      DELETE wnfstx WHERE taxtyp  = 'ICMO'.
    ENDIF.
  ENDIF.



  IF ( sy-tcode EQ 'VF01' OR sy-tcode EQ 'ZLES0077' OR sy-tcode EQ 'ZLES0136' OR sy-tcode EQ 'ZMM0127' OR
       sy-tcode EQ 'ZNFE' OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
       lc_ch_referencia   IS NOT INITIAL  )        AND "*-#143658-12.07.2024-JT
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*     'ZTRO_ZTAG_ZTAF_ZTAM_ZTAB_ZTRH_ZTRT_ZCFR_ZFTE_ZCOP_ZPOR_ZELV_ZROB_ZSSF' cs wvbrk-fkart .

" Inicio - IR215099 - Ajuste doc faturamento ZSCO - 30/12/2024 - GGARAUJO1
*       'ZTRO_ZTAG_ZTAF_ZTAM_ZTAB_ZTRH_ZTRT_ZCFR_ZFTE_ZCOP_ZPOR_ZELV_ZWHE_ZROB_ZSSF' CS wvbrk-fkart.
       'ZSCO_ZTRO_ZTAG_ZTAF_ZTAM_ZTAB_ZTRH_ZTRT_ZCFR_ZFTE_ZCOP_ZPOR_ZELV_ZWHE_ZROB_ZSSF' CS wvbrk-fkart.
" Fim - IR215099 - Ajuste doc faturamento ZSCO - 30/12/2024 - GGARAUJO1
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
    DATA: wa_vbrp      TYPE vbrp,
          wa_vbak      TYPE vbak,
          wa_konv      TYPE konv,
          wa_konv_icms TYPE konv,
          wa_konv_cof  TYPE ty_konv_ztrt,
          wa_konv_pis  TYPE ty_konv_ztrt,
          wa_konv_rate TYPE konv,
          wa_vtfa      TYPE vtfa,
          wa_vfkp      TYPE vfkp,
          wa_vbkd      TYPE vbkd,
          xbase        TYPE f,
          xmbasico     TYPE f,
          x_zero       TYPE f,

          xmbexcl      TYPE f,
          xbase_aux    TYPE konv-kbetr,
          wa_vttk      TYPE vttk,
          ls_komv      TYPE komv,
          ls_tax       TYPE komv,
          t_konv_aux   TYPE TABLE OF konv WITH HEADER LINE.


    DATA: base   TYPE f, " Base de Calculo do Imposto
          rate   TYPE f, " Percentual do Imposto
          taxval TYPE f, " Valor do Imposto
          excbas TYPE f, " Base Excluida do Imposto
          othbas TYPE f. " Outras Bases

    SELECT SINGLE * FROM vbak
      INTO wa_vbak
      WHERE  vbeln  =  wvbrp-vgbel.

    LOOP AT wkomv.
      IF wkomv-kschl  =  'IBRX'.
        MOVE-CORRESPONDING wkomv TO wa_konv.
        EXIT.
      ENDIF.
    ENDLOOP.

    SELECT SINGLE * FROM vtfa
      INTO wa_vtfa
      WHERE vbelv  =  wa_vbak-tknum
      AND   vbtyp_v  =  '8'.

    SELECT SINGLE * FROM vfkp
      INTO wa_vfkp
      WHERE fknum  =  wa_vtfa-vbeln.

    TRY.

        cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
          EXPORTING it_selection_attribute = VALUE #(
         ( fieldname = 'KNUMV' value = wa_vfkp-knumv )
         ( fieldname = 'KSCHL' value = 'ZICM' )
         )
          IMPORTING et_prc_element_classic_format = DATA(etl120c6r472) ).
        wa_konv_icms = etl120c6r472[ 1 ].
      CATCH cx_prc_result cx_sy_itab_line_not_found .
        sy-subrc = 4.
    ENDTRY.

    "Modificação dos Impostos - Victor Hugo - 09.05.2013 - Inicio

    "BASE     Base de Calculo do Imposto
    "RATE     Percentual do Imposto
    "TAXVAL   Valor do Imposto
    "EXCBAS   Base Excluida do Imposto
    "OTHBAS   Outras Bases


    "Caso encontre a Condição ZICM na Price (KONV do Documento de Custo)
    "e Montante ou porcentagem da condição (KBETR) maior que Zero.

    IF ( sy-subrc EQ 0 ) AND ( wa_konv_icms-kbetr > 0 ).


      IF ( wa_konv_icms-kawrt IS INITIAL ).

        "Selecionar Documento de Transporte na VTTK para pegar o tipo do Frete
        SELECT SINGLE * FROM vttk INTO wa_vttk WHERE tknum EQ wa_vbak-tknum.

        IF ( sy-subrc EQ 0 ).

          CASE wa_vttk-shtyp.

              "Tipos de Frete
              "Z001 - Formação de Lote
              "Z004 - Formação de Lote Direto.
              "Z005 - Frete de Venda
              "Z020 - Frete de Transferencia
              "Z021 - Frete de Entrada

              CLEAR: base,rate,taxval,excbas,othbas.
            WHEN: 'Z001' OR 'Z004'.
              base   = 0.
              rate   = 0.
              taxval = 0.
              excbas = wa_konv-kawrt.
              othbas = 0.
            WHEN: 'Z005' OR 'Z020' OR 'Z021' OR 'Z025' OR 'Z009'.
              base   = 0.
              rate   = 0.
              taxval = 0.
              excbas = 0.
              othbas = wa_konv-kawrt.
          ENDCASE.

          LOOP AT wnfstx.
            IF wnfstx-taxtyp = 'ICM3'.
              MOVE: base   TO wnfstx-base,
                    rate   TO wnfstx-rate,
                    taxval TO wnfstx-taxval,
                    excbas TO wnfstx-excbas,
                    othbas TO wnfstx-othbas.
              MODIFY wnfstx. "Modifica a tabela dos Impostos
            ENDIF.
          ENDLOOP.

        ENDIF.


      ELSE.

        LOOP AT wkomv.
          IF wkomv-kschl  =  'ZICM'.
            MOVE-CORRESPONDING wkomv TO wa_konv_rate.
            EXIT.
          ENDIF.
        ENDLOOP.

        base   = wa_konv-kawrt. " IBRX
        rate   = wa_konv_rate-kbetr / 10. "ZICM - RATE
        taxval = wa_konv_icms-kawrt. " ZICM PRICE DO FRETE

        "Ini CS2017002070
        IF ( wa_vbak-tknum IS NOT INITIAL ) AND ( wa_vfkp-knumv IS NOT INITIAL ).

          CLEAR: t_konv_aux[].
*          select *
*            from konv INTO TABLE t_konv_aux
*           where knumv  = wa_vfkp-knumv
*             and kschl  = 'ZICM'.

          SELECT *
            FROM v_konv INTO TABLE @DATA(t_konv)
           WHERE knumv  = @wa_vfkp-knumv
             AND kschl  = 'ZICM'.

          MOVE-CORRESPONDING t_konv[] TO t_konv_aux[].

          IF t_konv_aux[] IS NOT INITIAL.
            CLEAR: taxval.
            LOOP AT t_konv_aux.
              ADD t_konv_aux-kawrt TO taxval.
            ENDLOOP.
          ENDIF.
        ENDIF.
        "Fim CS2017002070

        excbas = 0.
        othbas = 0.

        LOOP AT wnfstx.
          IF wnfstx-taxtyp = 'ICM3'.
            MOVE: base   TO wnfstx-base,
                  rate   TO wnfstx-rate,
                  taxval TO wnfstx-taxval,
                  excbas TO wnfstx-excbas,
                  othbas TO wnfstx-othbas.
            MODIFY wnfstx. "Modifica a tabela dos Impostos
          ENDIF.
        ENDLOOP.


      ENDIF.

    ELSE. " Não encontrada a condição ZICM na Price (KONV do documento de custo de Frete)

      IF ( wa_konv_icms-kawrt IS INITIAL ).

        "Selecionar Documento de Transporte na VTTK para pegar o tipo do Frete
        SELECT SINGLE * FROM vttk INTO wa_vttk WHERE tknum EQ wa_vbak-tknum.

        IF ( sy-subrc EQ 0 ).

          CASE wa_vttk-shtyp.

              "Tipos de Frete
              "Z001 - Formação de Lote
              "Z004 - Formação de Lote Direto.
              "Z005 - Frete de Venda
              "Z020 - Frete de Transferencia
              "Z021 - Frete de Entrada

              CLEAR: base,rate,taxval,excbas,othbas.
            WHEN: 'Z001' OR 'Z004'.
              base   = 0.
              rate   = 0.
              taxval = 0.
              excbas = wa_konv-kawrt.
              othbas = 0.
            WHEN: 'Z005' OR 'Z020' OR 'Z021' OR 'Z025' OR 'Z009'.
              base   = 0.
              rate   = 0.
              taxval = 0.
              excbas = 0.
              othbas = wa_konv-kawrt.
          ENDCASE.

          LOOP AT wnfstx.
            IF wnfstx-taxtyp = 'ICM3'.
              MOVE: base   TO wnfstx-base,
                    rate   TO wnfstx-rate,
                    taxval TO wnfstx-taxval,
                    excbas TO wnfstx-excbas,
                    othbas TO wnfstx-othbas.
              MODIFY wnfstx. "Modifica a tabela dos Impostos
            ENDIF.
          ENDLOOP.


          "Incluir ZIH1
          "BASE     Base de Calculo do Imposto
          "RATE     Percentual do Imposto
          "TAXVAL   Valor do Imposto
          "EXCBAS   Base Excluida do Imposto
          "OTHBAS   Outras Bases

          READ TABLE wkomv INTO ls_komv WITH KEY kschl = 'ZIH1'.
          IF ( sy-subrc EQ 0 ).

            wa_tx-taxtyp = 'INSS'.
            wa_tx-mandt  = sy-mandt.
            wa_tx-docnum = wvbrk-vbeln.
            wa_tx-itmnum = wvbrk-hipos.

            base   = ls_komv-kawrt.
            rate   = ( ( ls_komv-kbetr / 1000 ) * 100 ).
            taxval = ls_komv-kwert.
            excbas = 0.
            othbas = 0.

            MOVE: base   TO wa_tx-base,
                  rate   TO wa_tx-rate,
                  taxval TO wa_tx-taxval,
                  excbas TO wa_tx-excbas,
                  othbas TO wa_tx-othbas.


            APPEND wa_tx TO wnfstx. "Adicionar o ZIH1

          ENDIF.

        ELSE.
          LOOP AT wnfstx.
            IF wnfstx-taxtyp = 'ICM3'.
              IF wnfstx-excbas IS INITIAL.
                excbas = wnfstx-othbas.
                othbas = 0.
                MOVE: excbas TO wnfstx-excbas,
                      othbas TO wnfstx-othbas.
                MODIFY wnfstx. "Modifica a tabela dos Impostos
              ENDIF.
            ENDIF.
          ENDLOOP.


          READ TABLE wkomv INTO ls_komv WITH KEY kschl = 'ZIA1'.
          IF ( sy-subrc EQ 0 ).

            SELECT SINGLE * FROM vbkd INTO wa_vbkd WHERE vbeln EQ wa_vbak-vbeln.

            wa_tx-taxtyp = 'INSS'.
            wa_tx-mandt  = sy-mandt.
            wa_tx-docnum = wvbrk-vbeln.
            wa_tx-itmnum = wvbrk-hipos.

            CASE wa_vbak-waerk.
              WHEN: 'USD'.
                base   = ls_komv-kawrt * wa_vbkd-kurrf.
                taxval = ls_komv-kwert * wa_vbkd-kurrf.
              WHEN OTHERS.
                base   = ls_komv-kawrt.
                taxval = ls_komv-kwert.
            ENDCASE.

            rate   = ( ( ls_komv-kbetr / 1000 ) * 100 ).

            excbas = 0.
            othbas = 0.

            MOVE: base   TO wa_tx-base,
                  rate   TO wa_tx-rate,
                  taxval TO wa_tx-taxval,
                  excbas TO wa_tx-excbas,
                  othbas TO wa_tx-othbas.


            APPEND wa_tx TO wnfstx. "Adicionar o ZIH1

          ENDIF.

        ENDIF.
      ELSE.
        LOOP AT wnfstx.
          IF wnfstx-taxtyp = 'ICM3'.
            IF wnfstx-excbas IS INITIAL.
              excbas = wnfstx-othbas.
              othbas = 0.
              MOVE: excbas TO wnfstx-excbas,
                    othbas TO wnfstx-othbas.
              MODIFY wnfstx. "Modifica a tabela dos Impostos
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.
    ENDIF.


    "Incluir PIS e COFINS


    " Modificação dos Impostos - Victor Hugo - 09.05.2013 - Fim



*
*      if ( wa_konv2-kbetr > 0 ) and ( sy-subrc eq 0 ).
*
*       xbase = ( wa_konv2-kbetr / 10 ) .
*       xbase_aux = ( wa_konv2-kbetr / 10 ) .
*
*       if xbase_aux ne 100.
*
*          xbase = ( wa_konv2-kbetr / 1000 ).
*          xmbasico  = ( wa_konv-kawrt * xbase_aux ).
*          xmbexcl   =  wa_konv-kawrt  - xmbasico .
*
*          loop at wnfstx.
*            if wnfstx-TAXTYP = 'ICM3'.
*                move:  XMBASICO            to wnfstx-base,
*                       XMBEXCL             to wnfstx-excbas.
*                modify wnfstx.
*            endif.
*          endloop.
*
*       else.
*
*        if ( wa_konv2-kawrt is initial ).
*
*          select single * from vttk into wa_vttk where tknum eq wa_vbak-tknum.
*
*          if ( sy-subrc eq 0 ).
*            case wa_vttk-shtyp.
*              when: 'Z001' OR 'Z004'.
*                xmbasico = wa_konv-kawrt.
*
*               WHEN: 'Z005' OR 'Z020' OR 'Z021'.
*                 XMBEXCL = wa_konv-kawrt.
*            endcase.
*          endif.
*
*        loop at wnfstx.
*            if wnfstx-TAXTYP = 'ICM3'.
*                move: XMBEXCL     to wnfstx-othbas,
*                      xmbasico    to wnfstx-excbas,
*                      xmbasico    to wnfstx-BASE,
*                      xmbasico    to wnfstx-RATE.
*                modify wnfstx.
*            endif.
*          endloop.
*      else.
*
*        xmbasico  = 0.
*        xmbexcl   =  wa_konv-kawrt.
*
*        loop at wnfstx.
*            if wnfstx-TAXTYP = 'ICM3'.
*
*                move: XMBEXCL     to wnfstx-othbas,
*                      xmbasico    to wnfstx-excbas,
*                      xmbasico    to wnfstx-BASE,
*                      xmbasico    to wnfstx-RATE.
*                modify wnfstx.
*            endif.
*          endloop.
*
*       endif.
*    endif.
*else.
*        if ( wa_konv2-kawrt is initial ).
*
*          select single * from vttk into wa_vttk where tknum eq wa_vbak-tknum.
*
*          if ( sy-subrc eq 0 ).
*            case wa_vttk-shtyp.
*              when: 'Z001' OR 'Z004'.
*                 xmbasico = wa_konv-kawrt.
*                 x_zero = 0.
*               WHEN: 'Z005' OR 'Z020' OR 'Z021'.
*                 XMBEXCL = wa_konv-kawrt.
*                 x_zero = 0.
*
*            endcase.
*          endif.
*
*        loop at wnfstx.
*            if wnfstx-TAXTYP = 'ICM3'.
*                move: XMBEXCL   to wnfstx-othbas,
*                      xmbasico  to wnfstx-excbas,
*                      x_zero    to wnfstx-BASE,
*                      x_zero    to wnfstx-RATE.
*                modify wnfstx.
*            endif.
*          endloop.
*
*        endif.
*endif.


    " ALRS 11/12/2013 (estava duplicando a chave)
    SELECT *
       FROM setleaf
       INTO TABLE tl_setleaf
      WHERE setname EQ 'MAGGI_SD_IVA_PIS_COFINS'.

    SELECT SINGLE j_1btxsdc INTO vg_j_1btxsdc
       FROM vbap
      WHERE vbeln EQ wvbrp-vgbel
        AND posnr EQ wvbrp-vgpos.

    READ TABLE tl_setleaf WITH KEY valfrom = vg_j_1btxsdc.

    IF sy-subrc IS INITIAL.

      TRY.

          cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
            EXPORTING it_selection_attribute = VALUE #(
           ( fieldname = 'KNUMV' value = rvbak-knumv )
           )
            IMPORTING et_prc_element_classic_format = DATA(etl493c6r2043) ).
          CLEAR it_konv_ztrt.
          TYPES: BEGIN OF tyl493c6r34,
                   krech TYPE konv-krech,
                   kbetr TYPE konv-kbetr,
                   knumv TYPE konv-knumv,
                   kschl TYPE konv-kschl,
                   kawrt TYPE konv-kawrt,
                   kwert TYPE konv-kwert,
                 END OF tyl493c6r34.
          DATA: lml493c6r3739 TYPE tyl493c6r34,
                lwl493c6r9170 LIKE LINE OF it_konv_ztrt.
          LOOP AT etl493c6r2043 REFERENCE INTO DATA(ldrl493c6r8617).
            lml493c6r3739-krech = ldrl493c6r8617->krech.
            lml493c6r3739-kbetr = ldrl493c6r8617->kbetr.
            lml493c6r3739-knumv = ldrl493c6r8617->knumv.
            lml493c6r3739-kschl = ldrl493c6r8617->kschl.
            lml493c6r3739-kawrt = ldrl493c6r8617->kawrt.
            lml493c6r3739-kwert = ldrl493c6r8617->kwert.
            lwl493c6r9170 = lml493c6r3739.
            APPEND lwl493c6r9170 TO it_konv_ztrt.
          ENDLOOP.
        CATCH cx_prc_result .
          sy-subrc = 4.
      ENDTRY.

      IF sy-subrc IS INITIAL.
        excbas = 0.
        LOOP AT wnfstx.
          IF wnfstx-taxtyp = 'ICM3'.
            excbas = wnfstx-excbas.
            IF ( excbas = 0 ).
              excbas = wnfstx-base.
            ENDIF.
            EXIT.
          ENDIF.
        ENDLOOP.

        SELECT SINGLE * FROM vbak
            INTO wa_vbak
            WHERE  vbeln  =  wvbrp-vgbel.
        "ISSE
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*         if wvbrk-fkart = 'ZELV'.
" Inicio - IR215099 - Ajuste doc faturamento ZSCO - 30/12/2024 - GGARAUJO1
*        IF wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
        IF wvbrk-fkart = 'ZSCO' OR wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
" Fim - IR215099 - Ajuste doc faturamento ZSCO - 30/12/2024 - GGARAUJO1
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
          CLEAR: wa_konv_ztrt.
          wa_tx-taxtyp = 'ISSE'.
          READ TABLE it_konv_ztrt INTO wa_konv_ztrt WITH KEY kschl = 'ICMI'.
          IF wa_vbak-waerk = 'USD'.
            wa_konv_ztrt-kwert = wa_konv_ztrt-kwert * wvbrp-kursk.
          ENDIF.
          wa_tx-mandt  = sy-mandt.
          wa_tx-docnum = wvbrk-vbeln.
          wa_tx-itmnum = wvbrk-hipos.
          wa_tx-base   = wa_konv_ztrt-kwert.
          wa_tx-rate   = 5.
          wa_tx-taxval = wa_konv_ztrt-kwert * ( 5 / 100 ).
          wa_tx-othbas = 0.
          wa_tx-excbas = 0.
          APPEND wa_tx TO wnfstx.
        ENDIF.

        "Incluir PIS
        CLEAR: wa_konv_ztrt.
        wa_tx-taxtyp = 'IPSN'.

        READ TABLE it_konv_ztrt INTO wa_konv_ztrt WITH KEY kschl = 'ZCPI'.
        IF sy-subrc = 0 AND wa_konv_ztrt-kawrt GT 0 .
          READ TABLE it_konv_ztrt INTO wa_konv_pis WITH KEY kschl = 'ICMI'.
          IF wa_vbak-waerk = 'USD'.
            wa_konv_ztrt-kwert = wa_konv_ztrt-kwert * wvbrp-kursk.
            wa_konv_pis-kwert  = wa_konv_pis-kwert * wvbrp-kursk.
          ENDIF.
          IF excbas = 0.
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*         if wvbrk-fkart = 'ZELV'.
            IF wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
              excbas = wa_konv_pis-kwert.
            ELSE.
              excbas = wa_konv_pis-kbetr.
            ENDIF.
          ENDIF.
          wa_tx-mandt  = sy-mandt.
          wa_tx-docnum = wvbrk-vbeln.
          wa_tx-itmnum = wvbrk-hipos.
          wa_tx-base   = excbas.
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*         if wvbrk-fkart = 'ZELV'.
          IF wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
            wa_tx-rate   = ( wa_konv_ztrt-kwert / 1000 ) * 100.
          ELSE.
            wa_tx-rate   = ( wa_konv_ztrt-kbetr / 1000 ) * 100.
          ENDIF.
          wa_tx-taxval = wa_konv_ztrt-kawrt.
          wa_tx-othbas = 0.
          wa_tx-excbas = 0.

          IF wvbrk-fkart = 'ZTRH'.
            IF wa_konv_pis-kbetr > 0.
              CLEAR: vl_tax_val.
              vl_tax_val = ( wa_tx-rate * wa_konv_pis-kbetr ) / 100.
              IF vl_tax_val = wa_tx-taxval.
                wa_tx-base = wa_konv_pis-kbetr.
              ENDIF.
            ENDIF.
          ENDIF.

          APPEND wa_tx TO wnfstx.
        ELSE.
          READ TABLE it_konv_ztrt INTO wa_konv_ztrt WITH KEY kschl = 'ICMI'.
          IF wa_vbak-waerk = 'USD'.
            wa_konv_ztrt-kwert = wa_konv_ztrt-kwert * wvbrp-kursk.
          ENDIF.
          IF excbas = 0.
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*         if wvbrk-fkart = 'ZELV'.
            IF wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
              excbas = wa_konv_ztrt-kwert.
            ELSE.
              excbas = wa_konv_ztrt-kbetr.
            ENDIF.
          ENDIF.
          IF wvbrk-fkart = 'ZPOR' AND wa_vbak-waerk = 'USD'.
            READ TABLE wnfstx
             WITH KEY itmnum =  wvbrk-hipos
                      taxtyp = 'ISSE'.

            IF sy-subrc EQ 0.
              excbas = wnfstx-base.
            ENDIF.
          ENDIF.
          wa_tx-mandt  = sy-mandt.
          wa_tx-docnum = wvbrk-vbeln.
          wa_tx-itmnum = wvbrk-hipos.
          wa_tx-base   = 0.
          wa_tx-rate   = 0.
          wa_tx-taxval = 0.
          wa_tx-othbas = excbas.
          wa_tx-excbas = 0.

          IF wvbrk-fkart = 'ZTRH'.
            IF wa_konv_ztrt-kbetr > 0.
              wa_tx-othbas = wa_konv_ztrt-kbetr.
            ENDIF.
          ENDIF.

          APPEND wa_tx TO wnfstx.
        ENDIF.

        "Incluir Cofins
        CLEAR: wa_konv_ztrt.
        wa_tx-taxtyp = 'ICON'.
        READ TABLE it_konv_ztrt INTO wa_konv_ztrt WITH KEY kschl = 'ZCFI'.
        IF sy-subrc = 0 AND wa_konv_ztrt-kawrt GT 0 .
          READ TABLE it_konv_ztrt INTO wa_konv_cof WITH KEY kschl = 'ICMI'.
          IF wa_vbak-waerk = 'USD'.
            wa_konv_ztrt-kwert = wa_konv_ztrt-kwert * wvbrp-kursk.
            wa_konv_cof-kwert  = wa_konv_cof-kwert * wvbrp-kursk.
          ENDIF.
          IF excbas = 0.
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*         if wvbrk-fkart = 'ZELV'.
            IF wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
              excbas = wa_konv_cof-kwert.
            ELSE.
              excbas = wa_konv_cof-kbetr.
            ENDIF.
          ENDIF.
          wa_tx-mandt  = sy-mandt.
          wa_tx-docnum = wvbrk-vbeln.
          wa_tx-itmnum = wvbrk-hipos.
          wa_tx-base   = excbas.
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*         if wvbrk-fkart = 'ZELV'.
          IF wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
            wa_tx-rate   = ( wa_konv_ztrt-kwert / 1000 ) * 100.
          ELSE.
            wa_tx-rate   = ( wa_konv_ztrt-kbetr / 1000 ) * 100.
          ENDIF.
          wa_tx-taxval = wa_konv_ztrt-kawrt.
          wa_tx-othbas = 0.
          wa_tx-excbas = 0.

          IF wvbrk-fkart = 'ZTRH'.
            IF wa_konv_cof-kbetr > 0.
              CLEAR: vl_tax_val.
              vl_tax_val = ( wa_tx-rate * wa_konv_cof-kbetr ) / 100.
              IF vl_tax_val = wa_tx-taxval.
                wa_tx-base = wa_konv_cof-kbetr.
              ENDIF.
            ENDIF.
          ENDIF.

          APPEND wa_tx TO wnfstx.
        ELSE.
          READ TABLE it_konv_ztrt INTO wa_konv_ztrt WITH KEY kschl = 'ICMI'.
          IF wa_vbak-waerk = 'USD'.
            wa_konv_ztrt-kwert = wa_konv_ztrt-kwert * wvbrp-kursk.
          ENDIF.
          IF excbas = 0.
*Begin - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
*         if wvbrk-fkart = 'ZELV'.
            IF wvbrk-fkart = 'ZELV' OR wvbrk-fkart = 'ZWHE'.
*End - FMartins -  IR082764 - Impostos nfe ZWHE - 16/02/2022
              excbas = wa_konv_ztrt-kwert.
            ELSE.
              excbas = wa_konv_ztrt-kbetr.
            ENDIF.
          ENDIF.
          IF wvbrk-fkart = 'ZPOR' AND wa_vbak-waerk = 'USD'.
            READ TABLE wnfstx
             WITH KEY itmnum =  wvbrk-hipos
                      taxtyp = 'ISSE'.
            IF sy-subrc EQ 0.
              excbas = wnfstx-base.
            ENDIF.
          ENDIF.
          wa_tx-mandt  = sy-mandt.
          wa_tx-docnum = wvbrk-vbeln.
          wa_tx-itmnum = wvbrk-hipos.
          wa_tx-base   = 0.
          wa_tx-rate   = 0.
          wa_tx-taxval = 0.
          wa_tx-othbas = excbas.
          wa_tx-excbas = 0.

          IF wvbrk-fkart = 'ZTRH'.
            IF wa_konv_ztrt-kbetr > 0.
              wa_tx-othbas = wa_konv_ztrt-kbetr.
            ENDIF.
          ENDIF.

          APPEND wa_tx TO wnfstx.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDIF.




  "ALRS 14.01.15
*" Modificação de Imposto PIS/COFINS/ICMS - Victor Hugo - 05.04.2013 - Inicio
*if ( sy-tcode eq 'VF01' and wvbrk-fkart = 'ZTRT' ).
*
*REFRESH: it_konv_ztrt.
*clear: wa_konv_ztrt, wa_tx.
*
*DATA:  wa_konv_icm3 type konv.
*
*
*  DATA: tabix type sy-tabix.
*
*  select KRECH KBETR KNUMV KSCHL kawrt
*    from konv
*    into table it_konv_ztrt
*  where KNUMV eq   rvbak-knumv.
*
*  LOOP AT it_konv_ztrt into wa_konv_ztrt.
*
*     tabix = sy-tabix.
*    if ( wa_konv_ztrt-KRECH eq 'A' ).
*      wa_konv_ztrt-KBETR_N = ( ( wa_konv_ztrt-KBETR / 1000 ) * 100 ).
*      modify it_konv_ztrt from wa_konv_ztrt INDEX tabix.
*    endif.
*    clear: tabix.
*  ENDLOOP.
*
*  "Incluir PIS
*  CLEAR: wa_konv_ztrt.
*  wa_tx-TAXTYP = 'IPSN'.
*  READ TABLE it_konv_ztrt into wa_konv_ztrt with key KSCHL = 'ICMI'.
*  wa_tx-BASE   = wa_konv_ztrt-kbetr.
*
*  CLEAR: wa_konv_ztrt.
*  READ TABLE it_konv_ztrt into wa_konv_ztrt with key KSCHL = 'ZCPI'.
*  wa_tx-RATE   = wa_konv_ztrt-KBETR_N.
*  wa_tx-TAXVAL = wa_konv_ztrt-kawrt.
*
*  wa_tx-mandt  = sy-mandt.
*  wa_tx-docnum = wvbrk-vbeln.
*  wa_tx-itmnum = wvbrk-hipos.
*
*  append wa_tx to wnfstx.
*
*  "Incluir Cofins
*  CLEAR: wa_konv_ztrt.
*  wa_tx-TAXTYP = 'ICON'.
*  READ TABLE it_konv_ztrt into wa_konv_ztrt with key KSCHL = 'ICMI'.
*  wa_tx-BASE   = wa_konv_ztrt-kbetr.
*
*  CLEAR: wa_konv_ztrt.
*  READ TABLE it_konv_ztrt into wa_konv_ztrt with key KSCHL = 'ZCFI'.
*  wa_tx-RATE   = wa_konv_ztrt-KBETR_N.
*  wa_tx-TAXVAL = wa_konv_ztrt-kawrt.
*
*  wa_tx-mandt  = sy-mandt.
*  wa_tx-docnum = wvbrk-vbeln.
*  wa_tx-itmnum = wvbrk-hipos.
*
*  append wa_tx to wnfstx.
*
*"Pedágio
*if not ( rvbak-tknum is initial ).
*
*  CLEAR: wa_vtfa, wa_vfkp.
*
*  SELECT SINGLE * FROM vtfa into wa_vtfa WHERE vbelv   eq rvbak-tknum
*                                           and vbtyp_v eq '8'
*                                           and vbtyp_n eq 'a'.
*
*  SELECT SINGLE * FROM vfkp into wa_vfkp where fknum eq wa_vtfa-vbeln.
*
*
*  LOOP AT wnfstx.
*
*      if ( wnfstx-TAXTYP eq 'ICM3' ).
*
*        move 0 to wnfstx-othbas.
*
*        CLEAR: wa_konv_ztrt.
*        READ TABLE it_konv_ztrt into wa_konv_ztrt with key KSCHL = 'ICMI'.
*        move wa_konv_ztrt-kbetr to wnfstx-base.
*
*        CLEAR: wa_konv_ztrt.
*        READ TABLE it_konv_ztrt into wa_konv_ztrt with key KSCHL = 'ZICM'.
*        move wa_konv_ztrt-KBETR_N to wnfstx-rate.
*        move wa_konv_ztrt-kawrt   to wnfstx-taxval.
*
*        select single * from konv into wa_konv_icm3 where knumv eq wa_vfkp-knumv
*                                                      and KSCHL eq 'ZPED'.
*        move wa_konv_icm3-kbetr to wnfstx-excbas.
*
*        modify wnfstx.
*
*       endif.
*
*  ENDLOOP.
* endif.
*endif.
*" Modificação de Imposto PIS/COFINS/ICMS - Victor Hugo - 05.04.2013 - Fim


  IF sy-tcode EQ 'VF01' OR sy-tcode EQ 'VF02' OR sy-tcode EQ 'ZLES0106' OR sy-tcode EQ 'ZLES0115' OR sy-tcode EQ 'ZLES0136' OR sy-tcode EQ 'ZSDT0066' OR sy-tcode EQ 'ZLES0077' OR sy-tcode EQ 'ZMM0127' OR
     sy-tcode EQ 'ZNFE' OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
     lc_ch_referencia   IS NOT INITIAL. "*-#143658-12.07.2024-JT

    SELECT *
      FROM setleaf
      INTO TABLE tl_setleaf
     WHERE setname EQ 'MAGGI_SD_IVA_PIS_COFINS'.

    IF wvbrk-fkart = 'ZTRI' OR wvbrk-fkart = 'ZREM'.
      lips-vgbel = waubel.
      lips-vgpos  = 10.
    ENDIF.
    CLEAR vg_j_1btxsdc.
    SELECT SINGLE j_1btxsdc INTO vg_j_1btxsdc
      FROM vbap
     WHERE vbeln EQ lips-vgbel
       AND posnr EQ lips-vgpos.
    IF sy-subrc = 0.
      SELECT SINGLE auart INTO vg_auart
        FROM vbak
         WHERE vbeln EQ lips-vgbel.

      READ TABLE tl_setleaf WITH KEY valfrom = vg_j_1btxsdc.

      IF sy-subrc IS INITIAL.
        IF wnflin[] IS NOT INITIAL.
          READ TABLE wnfnad
           WITH KEY parvw = 'RG'.
          IF sy-subrc IS INITIAL.
            SELECT *
              FROM zsdt0035
              INTO TABLE tl_0035.
*               for all entries in wnflin
*               where matnr eq wnflin-matnr
*                  or kunnr eq wnfnad-parid
*                  or auart eq vg_auart.

            CLEAR: wnfstx.
            LOOP AT wnflin.
              READ TABLE tl_0035
                WITH KEY auart = vg_auart
                         kunnr = wnfnad-parid
                         matnr = wnflin-matnr.

              IF sy-subrc IS NOT INITIAL.
                READ TABLE tl_0035
                WITH KEY auart = vg_auart
                         kunnr = wnfnad-parid
                         matnr = space.

                IF sy-subrc IS NOT INITIAL.
                  READ TABLE tl_0035
                   WITH KEY auart = vg_auart
                            kunnr = space
                            matnr = wnflin-matnr.

                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE tl_0035
                    WITH KEY  auart = vg_auart
                              kunnr = space
                              matnr = space .
                  ENDIF.
                ENDIF.
              ENDIF.

              IF sy-subrc IS INITIAL.
                IF tl_0035-pis  EQ '1'.
                  MOVE: wnflin-docnum TO wnfstx-docnum,
                        wnflin-itmnum TO wnfstx-itmnum,
                        'IPSN'        TO wnfstx-taxtyp,
                        wnflin-netwr  TO wnfstx-excbas.

                  APPEND wnfstx.
                  CLEAR wnfstx.
                ELSEIF tl_0035-pis EQ '2'.
                  MOVE: wnflin-docnum TO wnfstx-docnum,
                        wnflin-itmnum TO wnfstx-itmnum,
                        'IPSN'        TO wnfstx-taxtyp,
                        wnflin-netwr  TO wnfstx-base.

                  APPEND wnfstx.
                  CLEAR wnfstx.
                ENDIF.

                IF tl_0035-cofins  EQ '1'.
                  MOVE: wnflin-docnum TO wnfstx-docnum,
                        wnflin-itmnum TO wnfstx-itmnum,
                        'ICON'        TO wnfstx-taxtyp,
                        wnflin-netwr  TO wnfstx-excbas.

                  APPEND wnfstx.
                  CLEAR wnfstx.
                ELSEIF tl_0035-cofins EQ '2'.
                  MOVE: wnflin-docnum TO wnfstx-docnum,
                        wnflin-itmnum TO wnfstx-itmnum,
                        'ICON'        TO wnfstx-taxtyp,
                        wnflin-netwr  TO wnfstx-base.

                  APPEND wnfstx.
                  CLEAR wnfstx.
                ENDIF.
              ELSE.
                MOVE: wnflin-docnum TO wnfstx-docnum,
                        wnflin-itmnum TO wnfstx-itmnum,
                        'IPSN'        TO wnfstx-taxtyp,
                        wnflin-netwr  TO wnfstx-othbas.

                APPEND wnfstx.
                CLEAR wnfstx.

                MOVE: wnflin-docnum TO wnfstx-docnum,
                         wnflin-itmnum TO wnfstx-itmnum,
                         'ICON'        TO wnfstx-taxtyp,
                         wnflin-netwr  TO wnfstx-othbas.

                APPEND wnfstx.
                CLEAR wnfstx.

              ENDIF.
*              if sy-subrc is initial.
*                move: wnflin-docnum to wnfstx-docnum,
*                      wnflin-itmnum to wnfstx-itmnum,
*                      'ICON'        to wnfstx-taxtyp,
*                      wnflin-netwr  to wnfstx-excbas.
*
*                append wnfstx.
*                clear wnfstx.
*
*                move: wnflin-docnum to wnfstx-docnum,
*                      wnflin-itmnum to wnfstx-itmnum,
*                      'IPSN'        to wnfstx-taxtyp,
*                      wnflin-netwr  to wnfstx-excbas.
*
*                append wnfstx.
*                clear wnfstx.
*              else.
*                move: wnflin-docnum to wnfstx-docnum,
*                      wnflin-itmnum to wnfstx-itmnum,
*                      'ICON'        to wnfstx-taxtyp,
*                      wnflin-netwr  to wnfstx-othbas.
*
*                append wnfstx.
*                clear wnfstx.
*
*                move: wnflin-docnum to wnfstx-docnum,
*                      wnflin-itmnum to wnfstx-itmnum,
*                      'IPSN'        to wnfstx-taxtyp,
*                      wnflin-netwr  to wnfstx-othbas.
*
*                append wnfstx.
*                clear wnfstx.
*              endif.

            ENDLOOP.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDENHANCEMENT.
