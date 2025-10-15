*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*TYPES:
*
*  BEGIN OF ty_mara,
*    material TYPE mara,
*    maktg   TYPE makt-maktg,
*    maktx   TYPE makt-maktx,
*  END OF ty_mara,
*
*
*  BEGIN OF ty_response,
*    material        TYPE ty_mara,
*    dados_centro    TYPE marc,
*    dados_deposito  TYPE mard,
*    dados_avaliacao TYPE mbew,
*  END OF ty_response.
*
*TYPES: zde_data_response TYPE ty_response .
