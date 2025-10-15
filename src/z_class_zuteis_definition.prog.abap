
*&---------------------------------------------------------------------*
*& CLASS ZUTEIS	DEFINITION                                             *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

  PUBLIC SECTION.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_HIERARCHY                                      *
*&---------------------------------------------------------------------*
    METHODS: Z_CHECAR_EQUI_HIERARCHY EXPORTING
                                        RETURN TYPE CHAR1.

*&---------------------------------------------------------------------*
*& METHOD Z_EQUI_HIERARCHY_READ                                        *
*&---------------------------------------------------------------------*
    METHODS: Z_EQUI_HIERARCHY_READ IMPORTING
                                     EQUIPMENT TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_STATUS_BAPIS                                      *
*&---------------------------------------------------------------------*
        METHODS: Z_ATUALIZA_STATUS_BAPIS IMPORTING
                                        TXT_STATUS TYPE ITEX132.

*&---------------------------------------------------------------------*
*& METHOD Z_CREATE_COSTCENTER                                          *
*&---------------------------------------------------------------------*
    METHODS: Z_CREATE_COSTCENTER IMPORTING
                                        SWERK1 TYPE IWERK
                                        SWERK2 TYPE IWERK
                                        CENTER TYPE KOSTL.

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_ZEROS                                               *
*&---------------------------------------------------------------------*
    METHODS: Z_DELETE_ZEROS CHANGING
                                         FIELD TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_INSERT_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    METHODS: Z_INSERT_DADOS_EMPRESTIMO IMPORTING
                                         EQUNR TYPE EQUNR
                                         SWERK TYPE SWERK
                                         IWERK TYPE IWERK
                                       QT_DIAS TYPE NUMC3
                                         ERDAT TYPE SY-DATUM
                                         UNAME TYPE SY-UNAME
                                         EQKTX TYPE KTX01
                                   NUMERO_NOTA TYPE QMNUM
                                   ORDEM_ABAST TYPE DAUFN
                                   ORDEM_REMON TYPE ILOM_ORDST.

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    METHODS: Z_DELETE_DADOS_EMPRESTIMO IMPORTING
                                       EQUIPMENT TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_DT_HR_DEVOLUCAO                                    *
*&---------------------------------------------------------------------*
    METHODS: Z_CHECAR_DT_HR_DEVOLUCAO EXPORTING
                                        RETURN TYPE CHAR1
                                      CHANGING
                                        IT_TAB TYPE ANY TABLE
                                        WA_TAB TYPE ANY.

    METHODS: Z_LIMPAR_TELA.

  PRIVATE SECTION.
    DATA: CONCAC_TEXT TYPE CHAR200.
