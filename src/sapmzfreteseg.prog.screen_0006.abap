
PROCESS BEFORE OUTPUT.

  MODULE status_0006.

  MODULE recupera_info.

  MODULE campos_visiveis.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zvalor_seg_terc-monat.
    FIELD zvalor_seg_terc-gjahr.
    FIELD zvalor_seg_terc-cd_pais_ini.
    FIELD zvalor_seg_terc-cd_cidade_ini.
    FIELD zvalor_seg_terc-cd_uf_ini.
    FIELD zvalor_seg_terc-cd_pais_fim.
    FIELD zvalor_seg_terc-cd_cidade_fim.
    FIELD zvalor_seg_terc-cd_uf_fim.
    FIELD zvalor_seg_terc-cd_material.
    "FIELD zvalor_seg_terc-cd_moeda.
    FIELD zvalor_seg_terc-vr_fre_ton.
    FIELD zvalor_seg_terc-vr_mer_ton.
    FIELD zvalor_seg_terc-vr_imp_ton.
    FIELD zvalor_seg_terc-vr_tot_ton.
    FIELD zvalor_seg_terc-dt_inicio.
    FIELD zvalor_seg_terc-dt_final.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_0006.

PROCESS ON VALUE-REQUEST.

  FIELD zvalor_seg_terc-cd_cidade_ini MODULE f_dinicio_taxjurcode.
  FIELD zvalor_seg_terc-cd_cidade_fim MODULE f_dfinal_taxjurcode.
