"Name: \PR:SAPLJ_1B_NFE\FO:MAP_TAX_ICMS\SE:BEGIN\EI
ENHANCEMENT 0 Z_MAP_TAX_ICMS.

   SELECT SINGLE *
     FROM tvarvc INTO @DATA(lwa_inactive)
    WHERE name = 'INATIVA_MAP_TAX_ICMS_ENH'.

   IF sy-subrc NE 0.

     PERFORM zf_map_tax_icms USING p_lineid ps_rfc_det_tax.

*--------------------------------------------------------------------------------*
*  Ajustes ICMS Partilha
*--------------------------------------------------------------------------------*
     TYPES: BEGIN OF ty_icms_inter_v1,
              vbcufdest      TYPE j_1btaxval,
              pfcpufdest     TYPE j_1btaxval,
              picmsufdest    TYPE j_1btaxval,
              picmsinter     TYPE j_1btaxval,
              picmsinterpart TYPE j_1btaxval,
              vfcpufdest     TYPE j_1btaxval,
              vicmsufdest    TYPE j_1btaxval,
              vicmsufremet   TYPE j_1btaxval,
            END OF ty_icms_inter_v1.

     DATA: v_icms_part_calc_v1 TYPE c,
           wl_icms_inter_v1    TYPE ty_icms_inter_v1.

     DELETE gt_rfc_tax_icms_par_400 WHERE id IS INITIAL.
     DELETE gt_rfc_tax_icms_par_400 WHERE id = p_lineid.

     CLEAR: gs_rfc_tax_icms_par_400.

     CLEAR: wl_icms_inter_v1.

     CALL FUNCTION 'ZCALC_ICMS_VENDA_INTERESTADUAL'
       EXPORTING
         i_docnum         = xmli-docnum
         i_itmnum         = xmli-itmnum
         i_cfop           = xmli-cfop
       IMPORTING
         e_calculado      = v_icms_part_calc_v1
       CHANGING
         c_vbcufdest      = wl_icms_inter_v1-vbcufdest
         c_pfcpufdest     = wl_icms_inter_v1-pfcpufdest
         c_picmsufdest    = wl_icms_inter_v1-picmsufdest
         c_picmsinter     = wl_icms_inter_v1-picmsinter
         c_picmsinterpart = wl_icms_inter_v1-picmsinterpart
         c_vfcpufdest     = wl_icms_inter_v1-vfcpufdest
         c_vicmsufdest    = wl_icms_inter_v1-vicmsufdest
         c_vicmsufremet   = wl_icms_inter_v1-vicmsufremet.

     IF v_icms_part_calc_v1 IS NOT INITIAL.
       gs_rfc_tax_icms_par_400-id                  = p_lineid.
       gs_rfc_tax_icms_par_400-v_bcuf_dest         = wl_icms_inter_v1-vbcufdest.
       gs_rfc_tax_icms_par_400-p_fcpuf_dest        = wl_icms_inter_v1-pfcpufdest.
       gs_rfc_tax_icms_par_400-p_icmsuf_dest       = wl_icms_inter_v1-picmsufdest.
       gs_rfc_tax_icms_par_400-p_icms_inter        = wl_icms_inter_v1-picmsinter.
       gs_rfc_tax_icms_par_400-p_icms_inter_part   = wl_icms_inter_v1-picmsinterpart.
       gs_rfc_tax_icms_par_400-v_fcpuf_dest        = wl_icms_inter_v1-vfcpufdest.
       gs_rfc_tax_icms_par_400-v_icmsuf_dest       = wl_icms_inter_v1-vicmsufdest.
       gs_rfc_tax_icms_par_400-v_icmsuf_remet      = wl_icms_inter_v1-vicmsufremet.

       APPEND gs_rfc_tax_icms_par_400 TO gt_rfc_tax_icms_par_400.
     ENDIF.

     EXIT.

   ENDIF.

ENDENHANCEMENT.
