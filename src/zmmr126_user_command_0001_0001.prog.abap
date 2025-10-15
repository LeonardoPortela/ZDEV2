*----------------------------------------------------------------------*
***INCLUDE ZMMR126_USER_COMMAND_0001.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code.
    WHEN 'PESQUISAR'.

      CALL SCREEN 0100 STARTING AT 20 02.
      CLEAR: ok_code.

    WHEN 'SELECAOPR'.
      PERFORM selecao_padrao.
      CLEAR: ok_code.
    WHEN 'NOVA_CARGA'.
      PERFORM nova_carga.
      CLEAR: ok_code.
    WHEN 'REP_00001'.
      nm_report = 'MM_0001'.
      CALL SCREEN 9005.
      CLEAR: ok_code.
    WHEN 'REP_00002'.
      nm_report = 'MM_0001'.
      nm_atual  = abap_true.
      CALL SCREEN 9005.
      CLEAR: ok_code.

    WHEN 'REP_00003'.

      CALL FUNCTION 'ZMF_RELA_INSTRU_OV_ALGO'
        EXPORTING
          i_nr_safra  = psafra
          i_id_bukrs  = pempre
          i_id_branch = pfilia.

    WHEN 'REP_00004'.

      CALL FUNCTION 'ZMF_RELA_INSTRU_OV_ALGO'
        EXPORTING
          i_nr_safra = psafra.

    WHEN 'NOVA_ALGO'.
*-CS2022000332-#78064-07.06.2022-JT-inicio
      TRY.
          zcl_carga_recebimento_v0001=>zif_carga~get_instance(
            )->set_validar_safra( EXPORTING i_safra = psafra
                                            i_acao  = cl_myevent_handler=>st_action_algo
            ).
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
      ENDTRY.
*-CS2022000332-#78064-07.06.2022-JT-fim

      SUBMIT zmmr153 WITH pck_cad  EQ abap_true
                     WITH psafra   EQ psafra
                     WITH pempre   EQ pempre
                     WITH pfilia   EQ pfilia AND RETURN.
    WHEN 'NOVA_SOLIC'.
      SUBMIT zmmr128 WITH psafra   EQ psafra
                     WITH pempre   EQ pempre
                     WITH pfilia   EQ pfilia AND RETURN.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.
