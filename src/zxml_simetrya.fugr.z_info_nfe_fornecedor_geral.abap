FUNCTION Z_INFO_NFE_FORNECEDOR_GERAL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  DATA: IT_ZIB_NFE_FORN TYPE TABLE OF ZIB_NFE_FORN INITIAL SIZE 0 WITH HEADER LINE,
        WA_ZIB_NFE_FORN TYPE ZIB_NFE_FORN,
        IT_J_1BNFDOC    TYPE TABLE OF J_1BNFDOC INITIAL SIZE 0 WITH HEADER LINE,
        WA_J_1BNFDOC    TYPE J_1BNFDOC,
        VG_DOCNUM       TYPE J_1BDOCNUM,
        VGDT_EMISSAO    TYPE ZIB_NFE_FORN-DT_EMISSAO,
        VGDT_BASE_AJUST TYPE ZIB_NFE_FORN-DT_EMISSAO.
  "VL_NU_CHAVE     type zib_nfe_forn-NU_CHAVE.

  RANGES VL_NU_CHAVE FOR ZIB_NFE_FORN-NU_CHAVE.

  VGDT_EMISSAO = SY-DATUM - 90.

  EXEC SQL.
    update SAPHANADB.zib_nfe_forn f
       set f.docnum     = '0000000000',
           f.atualizado = ' '
     where f.mandt      = :sy-mandt
       and f.atualizado = 'X'
       and f.dt_emissao >= :vgdt_emissao
       and f.docnum     <> '0000000000'
       and exists ( select * from SAPHANADB.j_1bnfdoc j where j.mandt = f.mandt and j.docnum = f.docnum and j.cancel||j.doctyp in ('X'||j.doctyp,' 5','X5')  )
  ENDEXEC.

*  SELECT * INTO TABLE it_zib_nfe_forn
*    FROM zib_nfe_forn
*   WHERE atualizado EQ 'X'
*     AND dt_emissao GE vgdt_emissao
*     AND docnum     NE space.
*
*  "Verifica se documento foi estornado*****************************************************************
*  "****************************************************************************************************
*
*  "delete it_zib_nfe_forn where dt_emissao lt vgdt_emissao.
*
*  "delete it_zib_nfe_forn where docnum is initial.
*
**  loop at it_zib_nfe_forn into wa_zib_nfe_forn.
**    if wa_zib_nfe_forn-docnum is initial.
**      delete it_zib_nfe_forn index sy-tabix.
**    endif.
**  endloop.
*
*  IF NOT it_zib_nfe_forn[] IS INITIAL.
*
*    SORT it_zib_nfe_forn BY docnum.
*
*    SELECT * INTO TABLE it_j_1bnfdoc
*      FROM j_1bnfdoc
*       FOR ALL ENTRIES IN it_zib_nfe_forn
*     WHERE docnum EQ it_zib_nfe_forn-docnum.
*
*    LOOP AT it_j_1bnfdoc INTO wa_j_1bnfdoc.
*      IF ( wa_j_1bnfdoc-cancel EQ c_x ) OR ( wa_j_1bnfdoc-doctyp EQ '5' ).
*        READ TABLE it_zib_nfe_forn INTO wa_zib_nfe_forn WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          CLEAR: wa_zib_nfe_forn-docnum,
*                 wa_zib_nfe_forn-atualizado." = 'C'.
*          MODIFY zib_nfe_forn FROM wa_zib_nfe_forn.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.
  "****************************************************************************************************
  "****************************************************************************************************

  CLEAR: IT_ZIB_NFE_FORN[],VL_NU_CHAVE.

  VGDT_EMISSAO = SY-DATUM - 120.

  IF VL_NU_CHAVE IS NOT INITIAL."Se precisar debugar algum caso especifico colocar um valor na variavel em tempo de execução
    SELECT * INTO TABLE IT_ZIB_NFE_FORN
      FROM ZIB_NFE_FORN
     WHERE NU_CHAVE IN VL_NU_CHAVE.
  ELSE.
    SELECT * INTO TABLE IT_ZIB_NFE_FORN
      FROM ZIB_NFE_FORN
     WHERE ATUALIZADO NE ABAP_TRUE
       AND DT_EMISSAO GE VGDT_EMISSAO.
  ENDIF.

  "DELETE it_zib_nfe_forn WHERE dt_emissao LT vgdt_emissao.

  SORT IT_ZIB_NFE_FORN BY ST_NOTA DT_EMISSAO.

  IF NOT IT_ZIB_NFE_FORN[] IS INITIAL.
    CALL FUNCTION 'Z_INFO_NFE_FORNECEDOR'
      TABLES
        IT_INFO_NOTAS = IT_ZIB_NFE_FORN.
  ENDIF.

ENDFUNCTION.
