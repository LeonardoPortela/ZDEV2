*&--------------------------------------------------------------------&*
*&                        AMAGGI                                      &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 14.04.2025                                              &*
*& Descrição: Limpar area trabalho BAPI MIRO                          &*
*&--------------------------------------------------------------------&*

*    FIELD-SYMBOLS: <fs_nfheader> TYPE any,
*                   <fs_rseg>     TYPE ANY TABLE,
*                   <fs_storno>   TYPE any.
*
*    CALL FUNCTION 'J_1B_IM_NF_REFRESH'.
*
*    ASSIGN ('(SAPLJ1BI)NFHEADER')    TO <fs_nfheader>.
*    IF sy-subrc = 0.
*      FREE <fs_nfheader>.
*    ENDIF.
*
*    ASSIGN ('(SAPLJ1BI)X4_RSEG[]')   TO <fs_rseg>.
*    IF sy-subrc = 0.
*      FREE <fs_rseg>.
*    ENDIF.
*
*    ASSIGN ('(SAPLJ1BI)STORNO_FLAG') TO <fs_storno>.
*    IF sy-subrc = 0.
*      FREE <fs_storno>.
*    ENDIF.
*
*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
