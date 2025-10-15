*&---------------------------------------------------------------------*
*&  Include           ZXVVFU01
*&---------------------------------------------------------------------*
*CVBRP

* MOD 12/10/2012 ARG Proyecto Lexp - Christian INI
data: vl_bstdk like vbkd-bstdk.
data: e_status(1),
      e_messa(64).

if sy-tcode = 'VF11'.
  call function 'Z_CONTROLE_FECHAMES'
    exporting
      i_bukrs  = vbrk-bukrs
      i_data   = vbrk-fkdat
    importing
      e_status = e_status
      e_messa  = e_messa
    exceptions
      error    = 1
      others   = 2.

  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  if  e_status = 'E'.
    message e000(z01) with e_messa.
  endif.
endif.

if ( sy-tcode eq 'VF11' ) or ( sy-tcode ne 'VF01' ).

  if vbrk-sfakn is not initial.

    select single * from j_1bnflin into @data(wa_j_1bnflin) where refkey eq @vbrk-sfakn.
    if sy-subrc is initial.
      select single * from zcte_ciot into @data(wa_zcte_ciot) where docnum eq @wa_j_1bnflin-docnum.
      if ( sy-subrc eq 0 ).
        if ( ( wa_zcte_ciot-st_ciot ne zcl_ciot=>c_8 ) and
             ( wa_zcte_ciot-st_ciot ne zcl_ciot=>c_0 ) or
             ( wa_zcte_ciot-st_ciot ne zcl_ciot=>c_0 ) and
             ( wa_zcte_ciot-st_ciot ne zcl_ciot=>c_8 ) ) and
           ( wa_zcte_ciot-st_ciot ne zcl_ciot=>c_9 ) and
           ( wa_zcte_ciot-st_ciot ne zcl_ciot=>c_3 ).
          message e000(z01) with 'Necessário cancelar a viagem'.
          stop.
        endif.
      endif.
    endif.

  endif.

endif.

if vbrk-fkart eq 'YV01' or
   vbrk-fkart eq 'YV03' or
   vbrk-fkart eq 'YV08' or
   vbrk-fkart eq 'YV10' .

  select single bstdk
  from vbkd
  into vl_bstdk
   where vbeln = cvbrp-aubel .

  if sy-subrc is initial.
    move vl_bstdk to xaccit-bldat.
  endif.

endif.
* MOD 12/10/2012 ARG Proyecto Lexp - Christian FIN

* "/ Disparo Hedge
zcl_webservice_tx_curva=>hedge_aquaviario(
  _code = sy-tcode
  _vbrk = vbrk
  _vbrp = cvbrp
).



*IF ( sy-tcode EQ 'VF01' AND SY-UCOMM EQ 'SICH' ) OR ( SY-TCODE EQ 'ZLES0136' AND SY-UCOMM EQ 'OPT2' ).
if ( sy-tcode eq 'VF01') or ( sy-tcode eq 'ZLES0136' ).
  data:
*            VG_DIF TYPE KOMV-KBETR,
    it_zsdt0339      type table of zsdt0339,
    ws_zsdt0339      type zsdt0339,
    vg_vlr_impst     type komv-kbetr,
    it_ZLOG_PRICE_OV type table of zlog_price_ov.

  clear: vg_vlr_impst.
  free: it_zsdt0339, it_zlog_price_ov.


  select * from zsdt0339
  into table it_zsdt0339
  where vbeln eq cvbrp-aubel.


  select * from vbap
  into table @data(gt_vbap)
  where vbeln eq @cvbrp-aubel.


  select *
  from prcd_elements into table @data(gt_prcd_elements)
   for all entries in @gt_vbap
  where knumv eq @gt_vbap-knumv_ana
  and kposn eq @gt_vbap-posnr
  and kschl in ('ICVA', 'ICBS ', 'ICMI').


  if sy-subrc eq 0.
    loop at gt_vbap assigning field-symbol(<ws_vbap>).
      clear: ws_zsdt0339, vg_vlr_impst.
*&-----------------------------------------------------------------------------------------------------------
*&    Validando valor unt do tipo ICMI / Se estiver na tabela de exceção não precisa validar.
*&-----------------------------------------------------------------------------------------------------------
      read table it_zsdt0339 into ws_zsdt0339 with key vbeln = <ws_vbap>-knumv_ana "Se estiver na tabela de exceção não validar o valor unt da ICMI / Transação ZSDT0338
                                                       kschl = 'ICMI'.


      if sy-subrc ne 0.
        read table gt_prcd_elements into data(ws_prcd_elements) with key knumv = <ws_vbap>-knumv_ana
                                                                         kposn = <ws_vbap>-posnr
                                                                         kschl = 'ICMI'.



        if sy-subrc eq 0.
          read table ckomv into data(ws_xkomv) with key kposn = ws_prcd_elements-kposn
                                                        waers = ws_prcd_elements-waers
                                                        kschl = 'ICMI'.
          if sy-subrc eq 0.

            "Registra log.
            it_zlog_price_ov = value #( ( vbeln = <ws_vbap>-vbeln
                              posnr = <ws_vbap>-posnr
                              kschl = ws_xkomv-kschl
                              waers = ws_xkomv-waers
                           knumv_ov = <ws_vbap>-knumv_ana
                           knumv_fa = ws_xkomv-knumv
                           kbetr_ov = ws_prcd_elements-kbetr
                           kbetr_fa = ws_xkomv-kbetr
                         dt_criacao = sy-datum
                         hr_criacao = sy-uzeit
                         us_criacao = sy-uname
                         tcode      = sy-tcode
                              ucomm = sy-ucomm
                              fkart = vbrk-fkart
                             pstyv  = cvbrp-pstyv
                          check_dif = ' '

              ) ).
            if it_zlog_price_ov is not initial.
              modify zlog_price_ov from table it_zlog_price_ov.
            endif.


            if ws_prcd_elements-waers eq 'BRL' and ws_xkomv-waers eq 'BRL'.
*            VG_DIF = LS_PRCD_ELEMENTS-KBETR - WS_XKOMV-KBETR.

*             IF VG_DIF > 1. "Tolerancia, caso seja maior que um real, informar erro para usuario.
              if ws_prcd_elements-kbetr ne ws_xkomv-kbetr.

                it_zlog_price_ov = value #( ( vbeln = <ws_vbap>-vbeln
                              posnr = <ws_vbap>-posnr
                              kschl = ws_xkomv-kschl
                              waers = ws_xkomv-waers
                           knumv_ov = <ws_vbap>-knumv_ana
                           knumv_fa = ws_xkomv-knumv
                           kbetr_ov = ws_prcd_elements-kbetr
                           kbetr_fa = ws_xkomv-kbetr
                         dt_criacao = sy-datum
                         hr_criacao = sy-uzeit
                         us_criacao = sy-uname
                         tcode      = sy-tcode
                              ucomm = sy-ucomm
                              fkart = vbrk-fkart
                             pstyv  = cvbrp-pstyv
                          check_dif = abap_true
              ) ).
                if it_zlog_price_ov is not initial.
                  modify zlog_price_ov from table it_zlog_price_ov.
                endif.

                message e024(sd) with 'O valor unt da condição ICMI da OV:' <ws_vbap>-vbeln
                                      'é diferente do valor unt da condição ICMI da fatura '
                                      'abrir FI e direcionar para area fiscal analisar' display like 'W'.
                stop.
              endif.
            endif.
          endif.
        endif.
      endif.
      clear: ws_prcd_elements, ws_xkomv, ws_zsdt0339.
*
**&-----------------------------------------------------------------------------------------------------------
**&    Validando valor unt do tipo ICVA / Se estiver na tabela de exceção não precisa validar.
**&-----------------------------------------------------------------------------------------------------------
*      read table it_zsdt0339 into ws_zsdt0339 with key vbeln = <ws_vbap>-knumv_ana "Se estiver na tabela de exceção não validar o valor unt da ICMI / Transação ZSDT0338
*                                                       kschl = 'ICVA'.
*
*
*      if sy-subrc ne 0.
*        read table gt_prcd_elements into ws_prcd_elements with key knumv = <ws_vbap>-knumv_ana
*                                                                         kposn = <ws_vbap>-posnr
*                                                                         kschl = 'ICVA'.
*
*
*        if sy-subrc eq 0.
*          read table ckomv into ws_xkomv with key kposn = ws_prcd_elements-kposn
*                                                  waers = ws_prcd_elements-waers
*                                                  kschl = 'ICVA'.
*          if sy-subrc eq 0.
*
*            "Registra log.
*            clear: vg_vlr_impst.
*            vg_vlr_impst = ws_xkomv-kbetr / 10.
*
*            it_zlog_price_ov = value #( ( vbeln = <ws_vbap>-vbeln
*                              posnr = <ws_vbap>-posnr
*                              kschl = ws_xkomv-kschl
*                              waers = ws_xkomv-waers
*                           knumv_ov = <ws_vbap>-knumv_ana
*                           knumv_fa = ws_xkomv-knumv
*                           kbetr_ov = ws_prcd_elements-kbetr
*                           kbetr_fa = vg_vlr_impst
*                         dt_criacao = sy-datum
*                         hr_criacao = sy-uzeit
*                         us_criacao = sy-uname
*                         tcode      = sy-tcode
*                              ucomm = sy-ucomm
*                              fkart = vbrk-fkart
*                             pstyv  = cvbrp-pstyv
*                          check_dif = ' '
*
*              ) ).
*            if it_zlog_price_ov is not initial.
*              modify zlog_price_ov from table it_zlog_price_ov.
*            endif.
*
**             IF VG_DIF > 1. "Tolerancia, caso seja maior que um real, informar erro para usuario.
*            vg_vlr_impst = ws_xkomv-kbetr / 10.
*            if ws_prcd_elements-kbetr ne vg_vlr_impst.
*
*              "Registra log.
*              it_zlog_price_ov = value #( ( vbeln = <ws_vbap>-vbeln
*                                posnr = <ws_vbap>-posnr
*                                kschl = ws_xkomv-kschl
*                                waers = ws_xkomv-waers
*                             knumv_ov = <ws_vbap>-knumv_ana
*                             knumv_fa = ws_xkomv-knumv
*                             kbetr_ov = ws_prcd_elements-kbetr
*                             kbetr_fa = vg_vlr_impst
*                           dt_criacao = sy-datum
*                           hr_criacao = sy-uzeit
*                           us_criacao = sy-uname
*                           tcode      = sy-tcode
*                                ucomm = sy-ucomm
*                                fkart = vbrk-fkart
*                               pstyv  = cvbrp-pstyv
*                            check_dif = ' '
*
*                ) ).
*              if it_zlog_price_ov is not initial.
*                modify zlog_price_ov from table it_zlog_price_ov.
*              endif.
*
*              message  e024(sd) with 'O valor unt da condição ICVA da OV:' <ws_vbap>-vbeln
*                                    'é diferente do valor unt da condição ICVA  da fatura '
*                                 'abrir FI e direcionar para area fiscal analisar' display like 'W'.
*              exit.
*            endif.
*          endif.
*        endif.
*      endif.
*      clear: ws_prcd_elements, ws_xkomv, ws_zsdt0339.


*&-----------------------------------------------------------------------------------------------------------
*&    Validando valor unt do tipo ICVA / Se estiver na tabela de exceção não precisa validar.
*&-----------------------------------------------------------------------------------------------------------
*      read table it_zsdt0339 into ws_zsdt0339 with key vbeln = <ws_vbap>-knumv_ana  "Se estiver na tabela de exceção não validar o valor unt da ICMI / Transação ZSDT0338
*                                                       kschl = 'ICBS'.
*
*
*      if sy-subrc ne 0.
*        read table gt_prcd_elements into ws_prcd_elements with key knumv = <ws_vbap>-knumv_ana
*                                                                         kposn = <ws_vbap>-posnr
*                                                                         kschl = 'ICBS'.
*
*
*
*        if sy-subrc eq 0.
*          read table ckomv into ws_xkomv with key kposn = ws_prcd_elements-kposn
*                                                  waers = ws_prcd_elements-waers
*                                                  kschl = 'ICBS'.
*          if sy-subrc eq 0.
*            clear: vg_vlr_impst.
*            vg_vlr_impst = ws_xkomv-kbetr / 10.
**            "Registra log.
*            it_zlog_price_ov = value #( ( vbeln = <ws_vbap>-vbeln
*                              posnr = <ws_vbap>-posnr
*                              kschl = ws_xkomv-kschl
*                              waers = ws_xkomv-waers
*                           knumv_ov = <ws_vbap>-knumv_ana
*                           knumv_fa = ws_xkomv-knumv
*                           kbetr_ov = ws_prcd_elements-kbetr
*                           kbetr_fa = vg_vlr_impst
*                         dt_criacao = sy-datum
*                         hr_criacao = sy-uzeit
*                         us_criacao = sy-uname
*                         tcode      = sy-tcode
*                              ucomm = sy-ucomm
*                              fkart = vbrk-fkart
*                             pstyv  = cvbrp-pstyv
*                          check_dif = ' '
*
*              ) ).
*            if it_zlog_price_ov is not initial.
*              modify zlog_price_ov from table it_zlog_price_ov.
*            endif.
*
*            vg_vlr_impst = ws_xkomv-kbetr / 10.
*            if ws_prcd_elements-kbetr ne vg_vlr_impst.
*
*              it_zlog_price_ov = value #( ( vbeln = <ws_vbap>-vbeln
*                          posnr = <ws_vbap>-posnr
*                          kschl = ws_xkomv-kschl
*                          waers = ws_xkomv-waers
*                       knumv_ov = <ws_vbap>-knumv_ana
*                       knumv_fa = ws_xkomv-knumv
*                       kbetr_ov = ws_prcd_elements-kbetr
*                       kbetr_fa = vg_vlr_impst
*                     dt_criacao = sy-datum
*                     hr_criacao = sy-uzeit
*                     us_criacao = sy-uname
*                     tcode      = sy-tcode
*                          ucomm = sy-ucomm
*                          fkart = vbrk-fkart
*                         pstyv  = cvbrp-pstyv
*                      check_dif = abap_true
*          ) ).
*              if it_zlog_price_ov is not initial.
*                modify zlog_price_ov from table it_zlog_price_ov.
*              endif.
*
*              message e024(sd) with 'O valor unt da condição ICBS da OV:' <ws_vbap>-vbeln
*                                    'é diferente do valor unt da condição ICBS da fatura '
*                                 'abrir FI e direcionar para area fiscal analisar'  display like 'W'.
*              exit.
*            endif.
*          endif.
*        endif.
*      endif.
      clear: ws_prcd_elements, ws_xkomv, ws_zsdt0339.
    endloop.
  endif.
endif.

*      IF IT_ZLOG_PRICE_OV IS NOT INITIAL.
*             MODIFY ZLOG_PRICE_OV FROM TABLE IT_ZLOG_PRICE_OV.
*      ENDIF.
*
**&----------------------------------------------------------------------------
**&    Fim ajuste USER STORY 138113/CS2024000320 / AOENNING.
*&----------------------------------------------------------------------------
