*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
    TYPES:
      BEGIN OF ty_makt,
        maktg TYPE makt-maktg,
        maktx TYPE makt-maktx,
      END OF ty_makt,

 BEGIN OF ty_mara,
            material TYPE MARA,
          END OF ty_mara,


BEGIN OF ty_dados_centro,
  dados_centro TYPE MARC,
  END OF ty_dados_centro,


BEGIN OF ty_dados_deposito,
  DADOS_DEPOSITO TYPE MARD,
   END OF ty_dados_DEPOSITO,


BEGIN OF ty_dados_avaliacao,
  dados_avaliacao TYPE MBEW,
END OF ty_dados_avaliacao,



      BEGIN OF ty_response,
        MATERIAL     TYPE ty_mara,
        textos       TYPE ty_makt,
        dados_centro   TYPE ty_dados_centro,
        dados_DEPOSITO TYPE ty_dados_deposito,
        DADOS_AVALIACAO    TYPE ty_dados_avaliacao,
      END OF ty_response.

    TYPES: zde_data_response TYPE ty_response .
