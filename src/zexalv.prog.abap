*&---------------------------------------------------------------------*
*&  Include           ZEXALV
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&----------------------------------------------------------------------*
* Exibindo informações ALV                                              *
* https://homol.s360.com.br                                             *
*&----------------------------------------------------------------------*

  IF rb_cad IS INITIAL.

    IF p_amost-high IS NOT INITIAL.

      DO .

        IF p_amost-low > p_amost-high.
          EXIT.
        ELSE.
          SELECT SINGLE *
          FROM ztpm_l_amost
          INTO @DATA(l_amost)
          WHERE numeroamostra EQ @p_amost-low.

          IF l_amost IS NOT INITIAL.
            ADD 1 TO p_amost-low.
            CONDENSE p_amost-low.

          ELSE.
            zcl_webservic_als=>listar_amostra( EXPORTING numeroamostra = p_amost-low ).
            ADD 1 TO p_amost-low.
            CONDENSE p_amost-low.
          ENDIF.
        ENDIF.
      ENDDO.

    ELSE.

      CLEAR: l_amost.
      LOOP AT  p_amost.
        SELECT SINGLE *
        FROM ztpm_l_amost
        INTO l_amost
        WHERE numeroamostra EQ p_amost-low.

        CHECK l_amost IS INITIAL.
        zcl_webservic_als=>listar_amostra( EXPORTING numeroamostra = p_amost-low ).
      ENDLOOP.
    ENDIF.

    IF rb_anal IS NOT INITIAL.
      obj_dados->z_sel_dados_amostra( ).
    ELSE.
      obj_dados->z_sel_dados_amostra_sint( ).
    ENDIF.

  ELSE.
    obj_dados->z_sel_dados_cadastro_amostra( ).
  ENDIF.
