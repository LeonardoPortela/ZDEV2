*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF10.
*----------------------------------------------------------------------*
*{   INSERT         DEVK9A22VX                                        1
*&---------------------------------------------------------------------*
*& Form fs_verifica_duplicidade
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WA_ZIB_NFE_DIST_TER_CHAVE_NFE
*&---------------------------------------------------------------------*
FORM fs_verifica_duplicidade USING p_chave_nfe TYPE zde_chave_doc_e
                                   p_nu_chave_for TYPE char44.

  DATA: lv_quantidade_conv TYPE j_1bnetqty,
        lv_conf            TYPE j_1bnetqty.

  TRY.

      SELECT SINGLE *
          FROM zib_nfe_dist_ter
          INTO @DATA(ls_nfe_dist_ter)
          WHERE chave_nfe = @p_chave_nfe
            AND cd_fina_emissao = '1'.
      IF sy-subrc = 0.

        SELECT SINGLE chave_nfe, chave_nfe_ref
          INTO @DATA(ls_dist_ref)
          FROM zib_nfe_dist_ref
          WHERE chave_nfe_ref = @p_nu_chave_for.

        IF sy-subrc = 0.

          SELECT SINGLE chave_nfe, numero
            FROM zib_nfe_dist_ter
            INTO @DATA(ls_dist_ter_num)
            WHERE chave_nfe = @ls_dist_ref-chave_nfe
            AND cd_fina_emissao = '4'.
          IF sy-subrc = 0.
            MESSAGE e398(00) WITH  'Erro - esta nota tem a nota'
                               ls_dist_ter_num-numero
                               'como devolução de mercadorias'
                         RAISING error.
          ENDIF.
        ENDIF.
      ENDIF.

**********************************************************************
**********************************************************************

IF sy-tcode ='MIRO' or sy-tcode ='ZMM0116' or sy-tcode ='MIGO'.  ""AHSS - #143691 - 24/07/2024 - Ajustado para bloqueare somente a MIGO e ZMM0116.

      SELECT COUNT(*)
        FROM zib_nfe_dist_ter
         WHERE chave_nfe = p_chave_nfe
            AND cd_fina_emissao = '4'.

      IF sy-subrc = 0.
        MESSAGE e398(00) WITH  'ERRO - Esta nota é de devolução de mercadoria'
                           RAISING error.
      ENDIF.
ENDIF.

    CATCH cx_root.
      MESSAGE e398(00) WITH  'ERROR: Validação duplicidade'
                      RAISING error.
      RETURN.
  ENDTRY.
ENDFORM.
*}   INSERT
