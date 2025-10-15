"Name: \PR:SAPLJ1BI\FO:WH_TAX_HANDLING\SE:BEGIN\EI
ENHANCEMENT 0 Z_INPUT_TAX.
*Inclusao de linhas de IPI e COFINS quando isento
  data: tl_0031 type table of zmmt0031 with header line,
        tl_setleaf type table of setleaf with header line,
        V_ITMNUM type J_1BNFSTX-ITMNUM,

        T_IVAS            TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
        VLIFNR            TYPE LFA1-LIFNR,
        XUFF              TYPE LFA1-REGIO,
        XUFC              TYPE LFA1-REGIO,
        XALF              TYPE J_1BTXIC1-RATE,
        XALC              TYPE J_1BTXIC1-RATE,
        XALIQ_DIF         TYPE J_1BTXIC1-RATE,
        VSEQ(10)          TYPE P,
        OREF              TYPE REF TO ZCL_MEMORY_NFE_INBOUND_HANDLE,
        VNUM(10).

  DATA(LC_CRIAR_PIS_COFINS) = ABAP_FALSE.

  if sy-tcode EQ 'MIRO'.
    LC_CRIAR_PIS_COFINS = ABAP_TRUE.
  ELSE.

    TRY.
        DATA(HANDLE) = ZCL_MEMORY_NFE_INBOUND=>ATTACH_FOR_READ( INST_NAME = CONV #( rbkpv-LIFNR && rbkpv-XBLNR ) ).
        OREF ?= HANDLE->ROOT.
        LC_CRIAR_PIS_COFINS = ABAP_TRUE.
        HANDLE->DETACH( ).
      CATCH CX_SHM_ATTACH_ERROR.
    ENDTRY.
  ENDIF.

  "CS2020001318 Cálculo do DIFAL - base dupla
  loop at LINEITEM.
    IF lineitem-mwskz = 'P8'.
       delete litax where TAXTYP = 'ICM1'.
       delete litax where TAXTYP = 'ICOT'.
    ENDIF.
  ENDLOOP.
  "CS2020001318 Cálculo do DIFAL - base dupla

  if LC_CRIAR_PIS_COFINS EQ ABAP_TRUE.

  "ALRS 29/11/2016 alterado CS2016000534
  if LINEITEM[] is not initial.
    select *
      from setleaf
      into table tl_setleaf
     where setname eq 'MAGGI_MM_IVA_PIS_COFINS'.

    if sy-subrc is initial.
     select *
       from ZMMT0031
       into table tl_0031
       for all entries in LINEITEM
        where matnr eq LINEITEM-matnr
          and lifnr eq rbkpv-lifnr.

        loop at LINEITEM.

          read table tl_setleaf transporting no fields
            with key valfrom = lineitem-mwskz.

          if sy-subrc is initial.

            read table tl_0031 transporting no fields
              with key matnr = lineitem-matnr.

            if sy-subrc is initial.
              read table litax transporting no fields
                with key taxtyp = 'IPIS'
                         itmnum  = lineitem-itmnum.
              if sy-subrc is not initial.
                V_ITMNUM = LINEITEM-ITMNUM.
                clear litax.
                litax-ITMNUM = V_ITMNUM.
                litax-taxtyp = 'IPIS'.
                litax-excbas = lineitem-NETWR. "rbkpv-RMWWR.
                clear: litax-othbas, litax-WHTCOLLCODE.
                append litax.
              endif.

              read table litax transporting no fields
                with key taxtyp = 'ICOF'
                         itmnum  = lineitem-itmnum.
              if sy-subrc is not initial.
                V_ITMNUM = LINEITEM-ITMNUM.
                clear litax.
                litax-ITMNUM = V_ITMNUM.
                litax-taxtyp = 'ICOF'.
                litax-excbas = lineitem-NETWR. "rbkpv-RMWWR.
                clear: litax-othbas, litax-WHTCOLLCODE.
                append litax.
              endif.
            else.
             read table litax transporting no fields
                with key taxtyp = 'IPIS'
                         itmnum  = lineitem-itmnum.
              if sy-subrc is not initial.
                V_ITMNUM = LINEITEM-ITMNUM.
                clear litax.
                litax-ITMNUM = V_ITMNUM.
                litax-taxtyp = 'IPIS'.
                litax-othbas = lineitem-NETWR. "rbkpv-RMWWR.
                clear: litax-excbas, litax-WHTCOLLCODE.
                append litax.
              endif.

              read table litax transporting no fields
                with key taxtyp = 'ICOF'
                         itmnum  = lineitem-itmnum.
              if sy-subrc is not initial.
                V_ITMNUM = LINEITEM-ITMNUM.
                clear litax.
                litax-ITMNUM = V_ITMNUM.
                litax-taxtyp = 'ICOF'.
                litax-othbas = lineitem-NETWR. "rbkpv-RMWWR.
                clear: litax-excbas, litax-WHTCOLLCODE.
                append litax.
              endif.
            endif.
          endif.
        endloop.
    endif.
  endif.
  " Checa diferencial de aliquota
      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          CLASS         = '0000'
          SETNR         = 'MAGGI_IVA_NLOC'
        TABLES
          SET_VALUES    = T_IVAS
        EXCEPTIONS
          SET_NOT_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      SORT T_IVAS BY FROM.
      loop at litax.
         at new itmnum.
          read table lineitem transporting no fields
            with key itmnum = litax-itmnum.

          if sy-subrc is initial.
            READ TABLE T_IVAS
             WITH KEY FROM = LINEITEM-MWSKZ.

            IF SY-SUBRC = 0.
               SELECT SINGLE REGIO
                 FROM LFA1
                 INTO XUFF
                 WHERE LIFNR = RBKPV-LIFNR.

               IF SY-SUBRC NE 0.
                   CONTINUE.
               ENDIF.

               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                 EXPORTING
                   INPUT  = LINEITEM-WERKS
                 IMPORTING
                   OUTPUT = VLIFNR.

               SELECT SINGLE REGIO
                 FROM LFA1
                 INTO XUFC
                 WHERE LIFNR = VLIFNR.

               IF SY-SUBRC NE 0.
                   CONTINUE.
               ENDIF.

               SELECT SINGLE RATE
                 FROM J_1BTXIC1
                 INTO XALF
                 WHERE LAND1      =  'BR'
                 AND   SHIPFROM  = XUFF
                 AND   SHIPTO    = XUFC.

               IF SY-SUBRC NE 0.
                 CONTINUE.
               ENDIF.

               SELECT SINGLE RATE
                 FROM J_1BTXIC1
                 INTO XALC
                 WHERE LAND1      =  'BR'
                 AND   SHIPFROM  = XUFC
                 AND   SHIPTO    = XUFC.

               IF SY-SUBRC NE 0.
                 CONTINUE.
               ENDIF.
               XALIQ_DIF = XALC - XALF.

               IF XALIQ_DIF NE 0.
                  read table litax transporting no fields
                    with key taxtyp = 'ICOP'.
                  if sy-subrc is not initial.
                     V_ITMNUM = litax-ITMNUM.
                    clear litax.
                    litax-ITMNUM = V_ITMNUM.
                    litax-taxtyp = 'ICOP'.
                    litax-base   = rbkpv-RMWWR.
                    litax-rate   = XALIQ_DIF.
                    litax-taxval = rbkpv-RMWWR * ( XALIQ_DIF / 100 ).
                    clear: litax-excbas,litax-othbas, litax-WHTCOLLCODE.
                    append litax.
                  endif.
               Endif.
             Endif.
          ENDIF.
         endat.
      endloop.

  endif.
ENDENHANCEMENT.
