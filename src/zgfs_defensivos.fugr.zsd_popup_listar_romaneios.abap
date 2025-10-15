FUNCTION zsd_popup_listar_romaneios.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NRO_CG) TYPE  ZNRO_CG
*"  EXPORTING
*"     VALUE(E_EXCLUIU_ROMANEIO) TYPE  CHAR1
*"----------------------------------------------------------------------

  FREE: g_custom_container_pop_5821,
        g_custom_container2,
        ctl_alv1_pop_5821,
        g_grid2,
        l_excluiu_rom,
        e_excluiu_romaneio,
        l_erro.

  vg_cg_para_rom = i_nro_cg.

  PERFORM f_selecao_romaneios USING i_nro_cg.

*-----------------------------
*-exibir Romaneios
*-----------------------------
  CALL SCREEN 0200 STARTING AT 07  02
                     ENDING AT 183 20.

  e_excluiu_romaneio = l_excluiu_rom.

ENDFUNCTION.
