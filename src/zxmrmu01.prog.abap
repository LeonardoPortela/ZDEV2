*&---------------------------------------------------------------------*
*&  Include           ZXMRMU01
*&---------------------------------------------------------------------*
*IF GF_INIT IS INITIAL.              " define gf_init in zxmrmtop
*
*  MOVE 'X' TO I_RM61B-SCRAP.
*  MOVE 'X' TO I_RM61B-ZPKNB.
*  MOVE I_RM61B TO E_RM61B.
*  MOVE 'X' TO GF_INIT.              " clear this field after posting
*
*ENDIF.
*
MOVE I_RM61B TO E_RM61B.
