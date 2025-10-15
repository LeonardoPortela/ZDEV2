*----------------------------------------------------------------------*
***INCLUDE ZLJ_1B_NFEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ZF_MAP_TAX_ICMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form zf_map_tax_icms using p_lineid       type n
                        ps_rfc_det_tax type j_1bnfe_s_rfc_det_tax.

  constants lc_nfe_complementar    type j_1bnfdoc-nftype value '2'. "3159385
  constants  lc_added_value_margin type j_1bnflin-modbc  value '0'. "3159385
  data:  zcl_nfe_print type ref to zcl_im_cl_nfe_print.
  field-symbols: <item>           type j1b_nf_xml_item.

  data: ls_rfc_tax_icms        type j_1bnfe_s_rfc_tax_icms,  "ICMS and ICMS ST Tax - Block N
        ls_rfc_tax_icms_400    type j_1bnfe_s_rfc_tax_icms_400, "2459713
        lv_taxsit              type char2,
        lv_use_modbc           type c value 'X',
        lv_use_vbc             type c value 'X',
        lv_use_picms           type c value 'X',
        lv_use_vicms           type c value 'X',
        lv_use_modbcst         type c value '',
        lv_use_pmvast          type c value '',
        lv_use_predbcst        type c value '',
        lv_use_vbcst           type c value '',
        lv_use_picmsst         type c value '',
        lv_use_vicmsst         type c value '',
        lv_use_vicmsdeson      type c value '',
        lv_use_motdesicms      type c value '',
        lv_use_predbc          type c value '',
        lv_use_vicmsop         type c value '',
        lv_use_pdif            type c value '',
        lv_use_vicmsdif        type c value '',
        lv_use_vbcstret        type c value '',
        lv_use_vicmsstret      type c value '',
        lv_use_ufst            type c value '',
        lv_use_pbcop           type c value '',
        lv_use_vbcstdest       type c value '',
        lv_use_vicmsstdest     type c value '',
        lv_cst                 type char2,
        lv_use_iest            type c value '',             "2074832
        lv_use_vbcfcp          type c value '',             "2459713
        lv_use_pfcp            type c value '',             "2459713
        lv_use_vfcp            type c value '',             "2459713
        lv_use_vbcfcpst        type c value '',             "2459713
        lv_use_pfcpst          type c value '',             "2459713
        lv_use_vfcpst          type c value '',             "2459713
        lv_use_pst             type c value '',             "2459713
        lv_use_vbcfcpstret     type c value '',             "2459713
        lv_use_pfcpstret       type c value '',             "2459713
        lv_use_vfcpstret       type c value '',             "2459713
        lv_use_effect_icms     type c value '',             "2663083
        lv_use_vicmssubstituto type c value '',
        lv_use_vicmsstdeson    type c value '',
        lv_use_motdesicmsst    type c value '',
        lv_use_pfcpdif         type c value '',
        lv_use_vfcpdif         type c value '',
        lv_use_vfcpefet        type c value '',
* <--- RSI - NT2023.001
        lv_use_adrem_icms_ret  type c value '',
        lv_use_v_icms_mono_ret type c value '',
        lv_use_mono_pdif       type c value '',
        lv_use_q_bc_mono       type c value '',
        lv_use_adrem_icms      type c value '',
        lv_use_v_icms_mono_op  type c value '',
        lv_use_v_icms_mono_dif type c value '',
        lv_use_v_icms_mono     type c value '',
* <--- RSI - NT2023.001
        lv_check_statistical   type c value 'X',
        lv_codigo_forn         type lifnr,
        lv_regio               type regio,
        lv_taxsit_regio        type rvari_val_255.

  data ls_item_tax type j_1bnfstx.
  data ls_item     type j_1bnflin.
  data ls_j1baj type j_1baj.
  data lv_speccon    type j_1bsdktyp.                       "2117096
  data ls_tax_picmsinter type j_1bnfe_s_badi_picmsinter.    "2261984

  clear gv_tax_grp_ii.                                      "2040234

  loop at wk_item_tax into ls_item_tax
         where itmnum = xmli-itmnum.                        "2026778
    clear ls_j1baj.                                         "2242395
    call function 'J_1BAJ_READ'
      exporting
        taxtype              = ls_item_tax-taxtyp
      importing
        e_j_1baj             = ls_j1baj
      exceptions
        not_found            = 1
        parameters_incorrect = 2
        others               = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    " if there's a II taxgroup, set global variable in order
    " to fill this group on XML later
    if ls_j1baj-taxgrp = gc_tax_grp_ii.                     "2040234
      gv_tax_grp_ii = 'X'.                                  "2040234
    endif.                                                  "2040234

    "do not insert taxes from offset conditions                  "1621044
    check ls_j1baj-taxoff is initial.                       "1621044

    check ls_j1baj-taxgrp = 'ICMS' or ls_j1baj-taxgrp = 'ICST'.

    read table wk_item into ls_item                         "2026778
        with key itmnum = xmli-itmnum.

    call function 'CONVERSION_EXIT_TXSIT_OUTPUT'
      exporting
        input  = ls_item-taxsit
      importing
        output = lv_taxsit.

* Future Delivery with Partilha (validity rule SEFAZ, number 698)
    if  ( ls_item_tax-stattx = abap_true ) and              "2252987
        ( ls_j1baj-taxgrp = c_icms ) and                    "2258173
        ( ls_j1baj-subdivision = 0 ).                       "2258173

      if  xmlh_310-id_dest = c_2 and                        "2263884
          xmlh_310-ind_final = c_1 and                      "2263884
          xmlh_310-ind_iedest = c_9.                        "2263884

        gs_rfc_tax_icms_par-id = p_lineid.                  "2252987
        gs_rfc_tax_icms_par-p_icms_inter = ls_item_tax-rate. "2258173
        gs_rfc_tax_icms_par-p_icms_inter_part = wk_header-partr. "2261984

        if ls_item_tax-rate = 0.                            "2261984
          read table xml_tax_picmsinter_t                   "2261984
                                into ls_tax_picmsinter      "2261984
                            with key itmnum = xmli-itmnum.  "2261984
          if sy-subrc is initial and                        "2261984
             ls_tax_picmsinter-p_icms_inter <> 0.           "2261984
            gs_rfc_tax_icms_par-p_icms_inter =              "2261984
                            ls_tax_picmsinter-p_icms_inter. "2261984
          endif.                                            "2261984
        endif.                                              "2261984

      endif.                                                "2263884
    endif.                                                  "2252987

** Check statistical if tax type is relevant
    if ls_item_tax-taxtyp = 'ICAP' or
       ls_item_tax-taxtyp = 'ICEP' or
       ls_item_tax-taxtyp = 'ICSP'.
      check ls_item_tax-stattx is initial.
    endif.

** Initialize all the icms flags.
    lv_use_modbc        = 'X'.                              "2040397
    lv_use_vbc          = 'X'.                              "2040397
    lv_use_picms        = 'X'.                              "2040397
    lv_use_vicms        = 'X'.                              "2040397
    lv_use_pfcp         = 'X'.                              "2459713
    lv_use_vfcp         = 'X'.                              "2459713
    lv_use_modbcst      = ''.                               "2040397
    lv_use_pmvast       = ''.                               "2040397
    lv_use_predbcst     = ''.                               "2040397
    lv_use_vbcst        = ''.                               "2040397
    lv_use_picmsst      = ''.                               "2040397
    lv_use_vicmsst      = ''.                               "2040397
    lv_use_vicmsdeson   = ''.                               "2040397
    lv_use_motdesicms   = ''.                               "2040397
    lv_use_predbc       = ''.                               "2040397
    lv_use_vicmsop      = ''.                               "2040397
    lv_use_pdif         = ''.                               "2040397
    lv_use_vicmsdif     = ''.                               "2040397
    lv_use_vbcstret     = ''.                               "2040397
    lv_use_vicmsstret   = ''.                               "2040397
    lv_use_ufst         = ''.                               "2040397
    lv_use_pbcop        = ''.                               "2040397
    lv_use_vbcstdest    = ''.                               "2040397
    lv_use_vicmsstdest  = ''.                               "2040397
    lv_use_vbcfcp       = ''.                               "2459713
    lv_use_vbcfcpst     = ''.                               "2459713
    lv_use_pfcpst       = ''.                               "2459713
    lv_use_vfcpst       = ''.                               "2459713
    lv_use_pfcpdif      = ''.
    lv_use_vfcpdif      = ''.
    lv_use_vfcpefet     = ''.
    lv_check_statistical = abap_true.
* <--- RSI - NT2023.001
    lv_use_adrem_icms_ret = ''.
    lv_use_v_icms_mono_ret = ''.
    lv_use_adrem_icms_ret = ''.
    lv_use_v_icms_mono_ret = ''.
    lv_use_mono_pdif = ''.
    lv_use_q_bc_mono = ''.
    lv_use_adrem_icms = ''.
    lv_use_v_icms_mono_op = ''.
    lv_use_v_icms_mono_dif = ''.
    lv_use_v_icms_mono = ''.
* <--- RSI - NT2023.001

    ls_rfc_tax_icms-id   = p_lineid.                        "2040397
    ls_rfc_tax_icms-orig = ls_item-matorg.
    lv_cst = lv_taxsit.
    case lv_taxsit.
      when c_00.
      when c_10.
        lv_check_statistical = abap_false.
      when c_20.
        lv_check_statistical = abap_false.
      when c_30.
        lv_check_statistical = abap_false.
      when c_40.
        lv_check_statistical = abap_false.
      when c_41.
        lv_taxsit = c_40.
        lv_check_statistical = abap_false.
      when c_50.
        lv_taxsit = c_40.
        lv_check_statistical = abap_false.
      when c_51.
* <--- RSI - NT2023.001
      when c_53.
*        lv_use_mono_pdif       = 'X'.
*        lv_use_q_bc_mono       = 'X'.
*        lv_use_adrem_icms      = 'X'.
*        lv_use_v_icms_mono_op  = 'X'.
*        lv_use_v_icms_mono_dif = 'X'.
*        lv_use_v_icms_mono     = 'X'.
      when '61'.
* <--- RSI - NT2023.001                                                             "2040397
      when c_60.                                            "2040397
      when c_70.
        lv_check_statistical = abap_false.
      when others.
        lv_taxsit = c_90.
        lv_cst = c_90.
        lv_check_statistical = abap_false.
    endcase.

    if lv_check_statistical = abap_true.
      check ls_item_tax-stattx is initial.
    endif.

    ls_rfc_tax_icms-cst = lv_cst.

    if ls_j1baj-taxgrp = 'ICMS'.
      case lv_taxsit.
        when c_10.
          lv_use_vbcfcp     = 'X'.                          "2459713
          lv_use_pfcp       = 'X'.                          "2459713
          lv_use_vfcp       = 'X'.                          "2459713
          lv_use_modbcst    = 'X'.
          lv_use_pmvast     = 'X'.
          lv_use_vicmsstdeson = 'X'.
          lv_use_motdesicmsst = 'X'.
        when c_20.
          lv_use_vicmsdeson = 'X'.
          lv_use_motdesicms = 'X'.
          lv_use_predbc     = 'X'.
          lv_use_vbcfcp     = 'X'.                          "2459713
        when c_30.
          lv_use_vbc        = ''.
          lv_use_picms      = ''.
          lv_use_vicms      = ''.
          lv_use_pfcp       = ''.                           "2459713
          lv_use_vfcp       = ''.                           "2459713
          lv_use_pmvast     = 'X'.
          lv_use_vicmsdeson = 'X'.
          lv_use_motdesicms = 'X'.
          lv_use_modbcst    = 'X'.                          "2090326
        when c_40.
          lv_use_modbc      = ''.
          lv_use_vbc        = ''.
          lv_use_picms      = ''.
          lv_use_vicms      = ''.
          lv_use_pfcp       = ''.                           "2459713
          lv_use_vfcp       = ''.                           "2459713
          lv_use_vicmsdeson = 'X'.
          lv_use_motdesicms = 'X'.
        when c_51.
          lv_use_predbc     = 'X'.
          lv_use_vicmsop    = 'X'.
          lv_use_pdif       = 'X'.
          lv_use_vicmsdif   = 'X'.
          lv_use_vbcfcp     = 'X'.                          "2459713
          lv_use_pfcpdif    = 'X'.
          lv_use_vfcpdif    = 'X'.
          lv_use_vfcpefet   = 'X'.
* ---> RSI - NT2023.001
        when c_53.
          lv_use_mono_pdif       = 'X'.
          lv_use_q_bc_mono       = 'X'.
          lv_use_adrem_icms      = 'X'.
          lv_use_v_icms_mono_op  = 'X'.
          lv_use_v_icms_mono_dif = 'X'.
          lv_use_v_icms_mono     = 'X'.
* <--- RSI - NT2023.001
        when c_60.
          lv_use_modbc      = ''.
          lv_use_vbc        = ''.                           "2040397
          lv_use_picms      = ''.                           "2040397
          lv_use_vicms      = ''.
          lv_use_vbcstret   = 'X'.
          lv_use_vicmsstret = 'X'.
          lv_use_pst        = 'X'.                          "2459713
          lv_use_vbcfcpstret = 'X'.                         "2459713
          lv_use_pfcpstret  = 'X'.                          "2459713
          lv_use_vfcpstret  = 'X'.                          "2459713
          lv_use_effect_icms = 'X'.                         "2663083
          lv_use_vicmssubstituto = 'X'.                     "2750485
* <--- RSI - NT2023.001
        when '61'.
          lv_use_adrem_icms_ret = 'X'.
          lv_use_v_icms_mono_ret = 'X'.
* <--- RSI - NT2023.001
        when c_70.
          lv_use_vbcfcp     = 'X'.                          "2459713
          lv_use_pfcp       = 'X'.                          "2459713
          lv_use_vfcp       = 'X'.                          "2459713
          lv_use_modbcst    = 'X'.
          lv_use_pmvast     = 'X'.
          lv_use_vicmsdeson = 'X'.
          lv_use_motdesicms = 'X'.
          lv_use_predbc     = 'X'.
          lv_use_vicmsstdeson = 'X'.
          lv_use_motdesicmsst = 'X'.
        when c_90.
          lv_use_pmvast     = 'X'.
          lv_use_vicmsdeson = 'X'.
          lv_use_motdesicms = 'X'.
          lv_use_predbc     = 'X'.
          lv_use_vbcfcp     = 'X'.                          "2459713
          lv_use_vicmsstdeson = 'X'.
          lv_use_motdesicmsst = 'X'.
      endcase.

      if  xmlh_310-id_dest = c_2 and                        "2263884
          xmlh_310-ind_final = c_1 and                      "2263884
          xmlh_310-ind_iedest = c_9.                        "2263884

        if ( ls_item_tax-rate is not initial and            "2674905
             ls_item_tax-base is not initial ) or           "2674905
             ls_item_tax-excbas is not initial.             "2674905
          gs_rfc_tax_icms_par-p_icms_inter_part   = wk_header-partr.       "2259911 2648283
        endif.                                              "2648283
                                                            "2259911
        case ls_j1baj-subdivision.                          "2259911
          when c_001. "ICMS Destination Partition                        "2259911
            gs_rfc_tax_icms_par-id                  = p_lineid. "2259911
            gs_rfc_tax_icms_par-v_bcuf_dest         = ls_item_tax-base. "2259911
            gs_rfc_tax_icms_par-v_icmsuf_dest       = ls_item_tax-taxval. "2259911
            gs_rfc_tax_icms_par-p_icmsuf_dest       = ls_item_tax-rate. "2259911
            continue.                                       "2259911
          when c_002. "ICMS Origin Partition                             "2259911
            gs_rfc_tax_icms_par-id                  = p_lineid. "2259911
            gs_rfc_tax_icms_par-v_icmsuf_remet      = ls_item_tax-taxval. "2259911
            continue.                                       "2259911
          when c_003. "ICMS Special Fund                                 "2259911
            gs_rfc_tax_icms_par-id                  = p_lineid. "2259911
            gs_rfc_tax_icms_par-p_fcpuf_dest        = ls_item_tax-rate. "2259911
            gs_rfc_tax_icms_par-v_fcpuf_dest        = ls_item_tax-taxval. "2259911

            gs_rfc_tax_icms_par_400-v_bcfcpuf_dest  = ls_item_tax-base. "2459713
            continue.                                       "2262477
          when others.                                      "2259911
            gs_rfc_tax_icms_par-p_icms_inter        = ls_item_tax-rate. "2259911
        endcase.                                            "2259911
      endif.                                                "2259911

      if ls_j1baj-subdivision is initial and                "2261984
         ls_item_tax-rate = 0.                              "2261984
        read table xml_tax_picmsinter_t                     "2261984
                              into ls_tax_picmsinter        "2261984
                          with key itmnum = xmli-itmnum.    "2261984
        if sy-subrc is initial and                          "2261984
           ls_tax_picmsinter-p_icms_inter <> 0.             "2261984
          gs_rfc_tax_icms_par-id = p_lineid.                "2261984
          gs_rfc_tax_icms_par-p_icms_inter =                "2261984
                          ls_tax_picmsinter-p_icms_inter.   "2261984
        endif.                                              "2261984
      endif.                                                "2261984

      if not ls_j1baj-subdivision is initial and            "2459713
        ls_j1baj-subdivision < gc_subdivision_fcp.          "2459713
        continue.                                           "2263884
      endif.                                                "2263884

*>>>>>>>>=====Inicio melhoria chamado cs2024001079 / 158315 aoenning<<<<<<<<<<<<
      clear: lv_codigo_forn, lv_regio, lv_taxsit_regio.
      select single branch
      from j_1bnfdoc
      into lv_codigo_forn
        where docnum eq ls_item_tax-docnum.
      if sy-subrc eq 0.
        lv_codigo_forn = |{ lv_codigo_forn alpha = in }|.
        condense lv_codigo_forn no-gaps.

        "Seleciona o estado / regio.
        select single regio
        from lfa1
        into lv_regio
        where lifnr eq  lv_codigo_forn.
        if sy-subrc eq 0.
          "Concatena taxsit e regio.
          lv_taxsit_regio = |{ lv_taxsit }-{ lv_regio }|.
          condense  lv_taxsit_regio no-gaps.

          "Verificar STVARV => CST_UF_SEM_ICMS
          select single *
          from tvarvc
          into @data(wa_tvarvc)
            where name  eq 'CST_UF_SEM_ICMS'
              and low   eq @lv_taxsit_regio.
          if sy-subrc eq 0.
            "Limpar os valores das variaveis.
            lv_use_vbc      = ''.
            lv_use_picms    = ''.
            lv_use_vicms    = ''.
            lv_use_predbc   = ''.
            lv_use_vicmsop  = ''.
            lv_use_pdif     = ''.
            lv_use_vicmsdif = ''.
            lv_use_vbcfcp   = ''.
            lv_use_pfcpdif  = ''.
            lv_use_vfcpdif  = ''.
            lv_use_vfcpefet = ''.
          endif.
        endif.
      endif.
*>>>>>>>>=====Fim melhoria chamado CS2024001079 / 158315 AOENNING<<<<<<<<<<<<

    elseif ls_j1baj-taxgrp = 'ICST'.
      lv_use_modbc       = ''.
      lv_use_vbc         = ''.
      lv_use_vicms       = ''.
      lv_use_picms       = ''.
      lv_use_vbcfcp      = ''.                              "2459713
      lv_use_pfcp        = ''.                              "2459713
      lv_use_vfcp        = ''.                              "2459713
      lv_use_modbcst     = 'X'.                             "2113519
      lv_use_pmvast      = 'X'.                             "2051969
      lv_use_predbcst    = 'X'.                             "2008167
      lv_use_vbcst       = 'X'.                             "2008167
      lv_use_picmsst     = 'X'.                             "2008167
      lv_use_vicmsst     = 'X'.                             "2008167
      lv_use_vbcstret    = 'X'.
      lv_use_vicmsstret  = 'X'.
      lv_use_vbcstdest   = 'X'.
      lv_use_vicmsstdest = 'X'.
      lv_use_iest        = 'X'.                             "2074832
      lv_use_vbcfcpst    = 'X'.                             "2459713
      lv_use_pfcpst      = 'X'.                             "2459713
      lv_use_vfcpst      = 'X'.                             "2459713
      lv_use_pst         = 'X'.                             "2750485
      lv_use_vicmssubstituto = 'X'.                         "2750485
      lv_use_vbcfcpstret = 'X'.                             "2750485
      lv_use_pfcpstret   = 'X'.                             "2750485
      lv_use_vfcpstret   = 'X'.                             "2750485
      lv_use_effect_icms = 'X'.                             "2750485

    endif.

    if lv_check_statistical = abap_false and ls_item_tax-stattx = abap_true.

      lv_use_vbc             = abap_false.
      lv_use_picms           = abap_false.
      lv_use_vicms           = abap_false.
      lv_use_pfcp            = abap_false.
      lv_use_vfcp            = abap_false.
      lv_use_predbc          = abap_false.
      lv_use_vbcfcp          = abap_false.
      lv_use_pmvast          = abap_false.
      lv_use_vbcfcp          = abap_false.
      lv_use_predbcst        = abap_false.
      lv_use_vbcst           = abap_false.
      lv_use_picmsst         = abap_false.
      lv_use_vicmsst         = abap_false.
      lv_use_vbcstret        = abap_false.
      lv_use_vicmsstret      = abap_false.
      lv_use_vbcstdest       = abap_false.
      lv_use_vicmsstdest     = abap_false.
      lv_use_vbcfcpst        = abap_false.
      lv_use_pfcpst          = abap_false.
      lv_use_vfcpst          = abap_false.
      lv_use_vicmssubstituto = abap_false.
      lv_use_vbcfcpstret     = abap_false.
      lv_use_pfcpstret       = abap_false.
      lv_use_vfcpstret       = abap_false.
      lv_use_effect_icms     = abap_false.
      if lv_taxsit <> c_20 and lv_taxsit <> c_70 and lv_taxsit <> c_10.
        lv_use_modbc = abap_false.
      endif.
      if lv_taxsit <> c_30 and lv_taxsit <> c_10 and lv_taxsit <> c_70.
        lv_use_modbcst = abap_false.
      endif.

    endif.

    if ls_j1baj-subdivision = gc_subdivision_fcp.           "2459713
                                                            "2459713
      if lv_taxsit = c_51.

        if lv_use_vbcfcp = 'X'.
          if ls_item_tax-base is not initial.
            ls_rfc_tax_icms_400-v_bcfcp = ls_item_tax-base.
          elseif ls_item_tax-othbas is not initial.
            ls_rfc_tax_icms_400-v_bcfcp = ls_item_tax-othbas.
          endif.
        endif.

        if lv_use_pfcp = 'X'.
          ls_rfc_tax_icms_400-p_fcp = xmli_310-pfcp.
        endif.

        if lv_use_vfcp = 'X'.
          ls_rfc_tax_icms_400-v_fcp = xmli_310-vfcp.
        endif.

        if lv_use_pfcpdif = 'X'.
          ls_rfc_tax_icms_400-p_fcpdif = xmli_310-pfcpdif.
        endif.

        if lv_use_vfcpdif = 'X'.
          ls_rfc_tax_icms_400-v_fcpdif = xmli_310-vfcpdif.
        endif.

        if lv_use_vfcpefet = 'X'.
          ls_rfc_tax_icms_400-v_fcpefet = xmli_310-vfcpefet.
        endif.

      else.

        if lv_use_vbcfcp = 'X'.                             "2459713
          ls_rfc_tax_icms_400-v_bcfcp = ls_item_tax-base.   "2459713
        endif.                                              "2459713
                                                            "2459713
        if lv_use_pfcp = 'X'.                               "2459713
          ls_rfc_tax_icms_400-p_fcp = ls_item_tax-rate.     "2459713
        endif.                                              "2459713
                                                            "2459713
        if lv_use_vfcp = 'X'.                               "2459713
          ls_rfc_tax_icms_400-v_fcp = ls_item_tax-taxval.   "2459713
        endif.                                              "2459713
                                                            "2459713
        if lv_use_vbcfcpst = 'X'.                           "2459713
          ls_rfc_tax_icms_400-v_bcfcpst = ls_item_tax-base. "2459713
        endif.                                              "2459713
                                                            "2459713
        if lv_use_pfcpst = 'X'.                             "2459713
          ls_rfc_tax_icms_400-p_fcpst = ls_item_tax-rate.   "2459713
        endif.                                              "2459713
                                                            "2459713
        if lv_use_vfcpst = 'X'.                             "2459713
          ls_rfc_tax_icms_400-v_fcpst = ls_item_tax-taxval. "2459713
        endif.                                              "2459713

      endif.

      continue.                                             "2459713

    endif.                                                  "2459713

    if lv_use_vbc = 'X'.
      if ls_rfc_tax_icms-v_bc is initial or ls_rfc_tax_icms-v_bc < 0. "2008167
        if lv_taxsit = c_51 and                             "2145017
           xmli_310-vicmsdif is not initial.                "2145017
*normal base filled take base amount from there           "2145017
          if not ls_item_tax-base is initial.               "2145017
            ls_rfc_tax_icms-v_bc = ls_item_tax-base.        "2145017
*normal base empty - other based filled-> take other base "2145017
          elseif not ls_item_tax-othbas is initial.         "2145017
            ls_rfc_tax_icms-v_bc = ls_item_tax-othbas.      "2145017
*if using other base, add value to vBC tag (total)        "2145017
            add ls_item_tax-othbas to gs_rfc_icmstot-v_bc.  "2145017
            add ls_item_tax-othbas to gs_rfc_icmstot_400-v_bc. "2459713
          endif.                                            "2145017
        else.                                               "2145017
          perform fill_base using ls_item_tax-base
                              ls_item_tax-othbas
                              ls_item_tax-excbas
                         changing ls_rfc_tax_icms-v_bc.
        endif.                                              "2145017
      endif.                                                "2008167
    endif.

    if lv_use_picms = 'X'.
      if ls_rfc_tax_icms-p_icms is initial or ls_rfc_tax_icms-p_icms < 0. "2008167
        if lv_taxsit = c_51.                                "2852146
          ls_rfc_tax_icms-p_icms = xmli_310-picmsdef.       "2852146
        else.                                               "2852146
          ls_rfc_tax_icms-p_icms = ls_item_tax-rate.        "2852146
        endif.                                              "2852146
      endif.                                                "2008167
    endif.

    if lv_use_modbcst = 'X'.
      ls_rfc_tax_icms-mod_bcst = xmli_310-modbcst.          "2827564
    endif.

    if lv_use_vicms = 'X'.
      if ls_rfc_tax_icms-v_icms is initial or ls_rfc_tax_icms-v_icms < 0. "2008167
        ls_rfc_tax_icms-v_icms = ls_item_tax-taxval.
      endif.                                                "2008167
    endif.

    if lv_use_modbc = 'X'.                                  "2040397
      ls_rfc_tax_icms-mod_bc = ls_item-modbc.               "2040397
    endif.                                                  "3159385
                                                            "3159385
    if wk_header-doctyp = lc_nfe_complementar and           "3159385
          ls_item-modbc = ''.                               "3159385
                                                            "3159385
      ls_rfc_tax_icms-mod_bc =  lc_added_value_margin.      "3159385
    endif.                                                  "3159385
                                                            "3159385
    if lv_use_pmvast = 'X'.
      if xmli_310-p_mvast is not initial.                   "2827564
        ls_rfc_tax_icms-p_mvast = xmli_310-p_mvast.         "2827564
      endif.                                                "2827564
    endif.

    if lv_use_predbcst = 'X'.
      if ls_rfc_tax_icms-p_red_bcst is initial or ls_rfc_tax_icms-p_red_bcst < 0. "2008167
        if ls_item_tax-basered1 <> 0.                       "2135473
          ls_rfc_tax_icms-p_red_bcst = 100 - ls_item_tax-basered1. "2021156
        endif.                                              "2135473
      endif.                                                "2008167
    endif.

    if lv_use_vbcst = 'X'.
      if ( ls_rfc_tax_icms-cst = c_30 ).                    "2008167
        if ( ls_rfc_tax_icms-v_bcst is initial or ls_rfc_tax_icms-v_bcst < 0 ). "2008167
          perform fill_base using ls_item_tax-base
                                   ls_item_tax-othbas
                                   ls_item_tax-excbas
                             changing ls_rfc_tax_icms-v_bcst.
        endif.                                              "2008167
      else.                                                 "2090326
        perform fill_base using ls_item_tax-base
                                   ls_item_tax-othbas
                                   ls_item_tax-excbas
                             changing ls_rfc_tax_icms-v_bcst.
      endif.                                                "2008167
    endif.

    if lv_use_picmsst = 'X'.
      if ( ls_rfc_tax_icms-cst = c_30 ).                    "2008167
        if ( ls_rfc_tax_icms-p_icmsst is initial or ls_rfc_tax_icms-p_icmsst < 0 ). "2008167
          ls_rfc_tax_icms-p_icmsst = ls_item_tax-rate.
        endif.                                              "2008167
      else.                                                 "2090326
        ls_rfc_tax_icms-p_icmsst = ls_item_tax-rate.
      endif.                                                "2008167
    endif.

    if lv_use_vicmsst = 'X'.
      if ( ls_rfc_tax_icms-cst = c_30 ).                    "2008167
        if ( ls_rfc_tax_icms-v_icmsst is initial or ls_rfc_tax_icms-v_icmsst < 0 ). "2008167
          ls_rfc_tax_icms-v_icmsst = ls_item_tax-taxval.
        endif.                                              "2008167
      else.                                                 "2090326
        ls_rfc_tax_icms-v_icmsst = ls_item_tax-taxval.
      endif.                                                "2008167
    endif.

    if lv_use_motdesicms = 'X'.
      if xmli_310-motdeson is not initial.
        ls_rfc_tax_icms-mot_des_icms = xmli_310-motdeson.
      elseif xmli_badi is initial.                          "2645555
        ls_rfc_tax_icms-mot_des_icms = ls_item-motdesicms.
      endif.
    endif.


    "">>>>

    "LP-DRC - 30/11/2023 AJUSTE PARA PREENCHIMENDO VDESC QUANDO ZONA FRANCA-STANDARD JÁ ATENDE CASO PARAMETRIZADO verificar no futuro
    "Only fill vicmsdeson if mot_des_icms was filled, and avoid ICST and ICMS overwrite "2117096
    if lv_use_vicmsdeson = 'X' and                          "2117096
       ls_rfc_tax_icms-mot_des_icms is not initial and      "2117096
       ( ls_rfc_tax_icms-v_icmsdeson is initial or ls_rfc_tax_icms-v_icmsdeson < 0 ). "2117096
                                                            "2117096
      ls_rfc_tax_icms-v_icmsdeson = xmli_310-vicmsdeson.    "2117096
                                                            "2117096
      "For Zona Franca scenarios                                                        "2117096
      if gv_tax_free_zone = abap_true and                   "2117096
             ls_rfc_tax_icms-v_icmsdeson is initial and     "2117096
             gs_rfc_icmstot-v_desc is not initial.          "2117096
                                                            "2117096
        perform read_j_1bsdkonv                             "2117096
          using ls_item-reftyp ls_item-refkey               "2117096
          changing lv_speccon.                              "2117096
                                                            "2117096
        "When BXZF is mapped as discount                                                "2117096
        if lv_speccon = c_discount.                         "2117096
          "get value from ICZF to vicmsdeson                                            "2117096
          perform fill_vicmsdeson_zfm                       "2117096
            using ls_item_tax                               "2117096
            changing ls_rfc_tax_icms-v_icmsdeson.           "2117096
                                                            "2117096
          "remove icms deson from vdesc of item                                        "2117096
          xmli-vdesc = xmli-vdesc - ls_rfc_tax_icms-v_icmsdeson. "2117096
                                                            "2117096
          "remove icms deson from total of vdesc                                       "2117096
          gs_rfc_icmstot-v_desc = gs_rfc_icmstot-v_desc - ls_rfc_tax_icms-v_icmsdeson. "2117096
                                                            "2117096
          "add icmsdeson of item to total of deson                                     "2117096
          gs_rfc_icmstot-v_icmsdeson =                      "2117096
              gs_rfc_icmstot-v_icmsdeson + ls_rfc_tax_icms-v_icmsdeson. "2117096
                                                            "2117096
          move-corresponding gs_rfc_icmstot to gs_rfc_icmstot_400. "2654923
        endif.                                              "2117096
      endif.                                                "2117096
    endif.


    if gv_tax_free_zone is initial.

      create object zcl_nfe_print.
      call method zcl_nfe_print->seleci_knvi
        exporting
          p_docnum      = wk_header-docnum
          p_parid       = wk_header-parid
          p_empresa     = wk_header-bukrs
        importing
          p_zona_franca = data(lv_zfranca).

      gv_tax_free_zone = lv_zfranca.

    endif.

    assign ('(SAPLJ_1B_NFE)XMLI')          to <item>.
    if lv_use_vicmsdeson = 'X' .
      if gv_tax_free_zone = 'X' .
        data(lv_desc) = gs_rfc_icmstot-v_desc.
        clear: gs_rfc_icmstot-v_desc.

        gs_rfc_icmstot-v_desc = ls_rfc_tax_icms-v_icmsdeson + lv_desc .

        clear: gs_rfc_icmstot-v_prod.
        gs_rfc_icmstot-v_prod = gs_rfc_icmstot-v_nf + ls_rfc_tax_icms-v_icmsdeson + lv_desc .
        move-corresponding gs_rfc_icmstot to gs_rfc_icmstot_400.

      endif.

    endif.
**  <<<<<"END LP-VDEC 30/11/2023                                                "2117096

    if lv_use_vicmsstdeson = 'X' or lv_use_motdesicmsst = 'X'.
      ls_rfc_tax_icms_400-v_icmsstdeson  = xmli-vicmsstdeson.
      ls_rfc_tax_icms_400-mot_des_icmsst = xmli-motdesicmsst.
    endif.

    if lv_use_iest = 'X' or wk_header-partr <> 0.           "2260480
      perform map_iest.                                     "2074832
    endif.                                                  "2074832

    if lv_use_predbc = 'X'.
      if ls_rfc_tax_icms-p_red_bc is initial or ls_rfc_tax_icms-p_red_bc < 0.
        ls_rfc_tax_icms-p_red_bc = 100 - ls_item_tax-basered1.
      endif.
    endif.

    if lv_use_vicmsop = 'X'.
      if ls_rfc_tax_icms-v_icmsop is initial or ls_rfc_tax_icms-v_icmsop < 0.
        ls_rfc_tax_icms-v_icmsop = xmli_310-vicmsop.
      endif.
    endif.

    if lv_use_pdif = 'X'.
      if ls_rfc_tax_icms-p_dif is initial or ls_rfc_tax_icms-p_dif < 0.
        ls_rfc_tax_icms-p_dif = xmli_310-picmsdif.
      endif.
    endif.

    if lv_use_vicmsdif = 'X'.
      if ls_rfc_tax_icms-v_icmsdif is initial or ls_rfc_tax_icms-v_icmsdif < 0.
        ls_rfc_tax_icms-v_icmsdif = xmli_310-vicmsdif.
      endif.
    endif.

* ---> RSI - NT2020.005
*    IF lv_use_pfcpdif EQ 'X'.
*      IF ls_rfc_tax_icms-p_fcp_dif IS INITIAL OR ls_rfc_tax_icms-p_fcp_dif < 0.
*        ls_rfc_tax_icms-p_fcp_dif = xmli_310-pfcpdif.
*      ENDIF.
*    ENDIF.
*
*    IF lv_use_vfcpdif EQ 'X'.
*      IF ls_rfc_tax_icms-v_fcp_dif IS INITIAL OR ls_rfc_tax_icms-v_fcp_dif < 0.
*        ls_rfc_tax_icms-v_fcp_dif = xmli_310-vfcpdif.
*      ENDIF.
*    ENDIF.
*
*    IF ls_rfc_tax_icms-v_fcp_efet IS INITIAL OR ls_rfc_tax_icms-v_fcp_efet < 0.
*      ls_rfc_tax_icms-v_fcp_efet = xmli_310-vfcpefet.
*    ENDIF.
* <--- RSI - NT2020.005

    if lv_use_vbcstret = 'X'.
      if ls_rfc_tax_icms-v_bcstret is initial or ls_rfc_tax_icms-v_bcstret < 0. "2793825
        ls_rfc_tax_icms-v_bcstret = xmli_310-vbcstret.      "2793825
      endif.
    endif.

    if lv_use_vicmsstret = 'X'.
      if ls_rfc_tax_icms-v_icmsstret is initial or ls_rfc_tax_icms-v_icmsstret < 0.
        ls_rfc_tax_icms-v_icmsstret = xmli_310-vicmsstret.  "2040397
      endif.
    endif.

    if lv_use_pst = 'X'.                                    "2459713
      ls_rfc_tax_icms_400-p_st = xmli-pst.                  "2459713
    endif.                                                  "2459713
    if lv_use_vbcfcpstret = 'X'.                            "2459713
      ls_rfc_tax_icms_400-v_bcfcpstret = xmli-vbcfcpstret.  "2459713
    endif.                                                  "2459713
    if lv_use_pfcpstret = 'X'.                              "2459713
      ls_rfc_tax_icms_400-p_fcpstret = xmli-pfcpstret.      "2459713
    endif.                                                  "2459713
    if lv_use_vfcpstret = 'X'.                              "2459713
      ls_rfc_tax_icms_400-v_fcpstret = xmli-vfcpstret.      "2459713
    endif.                                                  "2459713

    if lv_use_ufst = 'X'.
      "ls_rfc_tax_icms-ufst = .
    endif.

    if lv_use_pbcop = 'X'.
      "ls_rfc_tax_icms-p_bcop = .
    endif.

    if lv_use_vbcstdest = 'X'.                              "2459713
      ls_rfc_tax_icms-v_bcstdest = xmli-vbcstdest.          "2459713
    endif.                                                  "2459713

    if lv_use_vicmsstdest = 'X'.                            "2459713
      ls_rfc_tax_icms-v_icmsstdest = xmli-vicmsstdest.      "2459713
    endif.                                                  "2459713

    if lv_use_effect_icms = 'X'.                            "2663083
      ls_rfc_tax_icms_400-p_red_bcefet = xmli_310-predbcefet. "2663083
      ls_rfc_tax_icms_400-v_bcefet     = xmli_310-vbcefet.  "2663083
      ls_rfc_tax_icms_400-p_icmsefet   = xmli_310-picmsefet. "2663083
      ls_rfc_tax_icms_400-v_icmsefet   = xmli_310-vicmsefet. "2663083
    endif.                                                  "2663083

    if lv_use_vicmssubstituto = 'X'.                        "2750485
      ls_rfc_tax_icms_400-v_icmssubstituto = xmli_310-vicmssubstituto. "2750485
    endif.                                                  "2750485

* ---> RSI - NT2023.001 - Descomentar apos equalização de request ambiente dev
    ""LP - utilizado desenvolvimento RIMINI, após configurãção price/determinação SD monofasico esse
    "alterar desenvolvimento abaixo, estando parametrizado não necessita deste desenvolvimento funcioaria standard
    if lv_use_adrem_icms_ret eq  'X'.
      " ls_rfc_tax_icms_400-adrem_icms_ret = wk_item-adrem_icms_ret.
      ls_rfc_tax_icms_400-ad_rem_icmsret = wk_item-adrem_icms_ret.
    endif.

    ls_rfc_tax_icms_400-q_bcmono_ret = wk_item-menge.

    if lv_use_v_icms_mono_ret eq  'X'.

      ls_rfc_tax_icms_400-v_icmsmono_ret = wk_item-v_icms_mono_ret.

      add ls_rfc_tax_icms_400-q_bcmono_ret to gs_rfc_icmstot_400-q_bcmono_ret.

    endif.

    if lv_use_mono_pdif eq 'X'.
      ls_rfc_tax_icms-p_dif = wk_item-p_dif.
    endif.

    if lv_use_q_bc_mono eq 'X'.

      ls_rfc_tax_icms_400-q_bcmono = wk_item-q_bc_mono.

      "ADD ls_rfc_tax_icms_400-Q_BCMONO TO gs_rfc_icmstot_400-Q_BCMONO.

    endif.

    if lv_use_adrem_icms eq 'X'.
      ls_rfc_tax_icms_400-ad_rem_icms = wk_item-adrem_icms.
    endif.

    if lv_use_v_icms_mono_op eq 'X'.
      ls_rfc_tax_icms_400-v_icmsmono_op = wk_item-v_icms_mono_op.
    endif.

    if lv_use_v_icms_mono_dif eq 'X'.
      ls_rfc_tax_icms_400-v_icmsmono_dif = wk_item-v_icms_mono_dif.
    endif.

    if lv_use_v_icms_mono eq 'X'.

      ls_rfc_tax_icms_400-v_icmsmono = wk_item-v_icms_mono.

      "ADD ls_rfc_tax_icms_400-V_ICMSMONO TO gs_rfc_icmstot_400-V_ICMSMONO.

    endif.

* <--- RSI - NT2023.001

    ps_rfc_det_tax-icms_ref      = p_lineid.
  endloop.

  perform append_structure_to_part_table.                   "2459713

  if ls_rfc_tax_icms is initial and                         "2459713
      not lv_taxsit is initial.                             "2459713
    ps_rfc_det_tax-icms_ref = p_lineid.                     "2125951
    ls_rfc_tax_icms-cst = lv_taxsit.                        "2087299
    ls_rfc_tax_icms-id   = p_lineid.                        "2087299
    ls_rfc_tax_icms-orig = ls_item-matorg.                  "2087299
  endif.                                                    "2087299

  perform append_structure_to_icms_table                    "2459713
    using ls_rfc_tax_icms ls_rfc_tax_icms_400.              "2459713

  if obj_ref is bound.                                      "2988631
    data lv_is_icms_part_exception type abap_bool.          "2988631
                                                            "2988631
    call method obj_ref->is_icms_part_in_exception_list         "2988631
      importing                                                 "2988631
        out_is_icms_part_exception = lv_is_icms_part_exception. "2988631
                                                            "2988631
    if lv_is_icms_part_exception = abap_true.               "2988631
      clear gt_rfc_tax_icms_par_400[].                      "2988631
    endif.                                                  "2988631
  endif.





endform.
