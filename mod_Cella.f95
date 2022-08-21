module mod_Cella

! by OV (11-11-2013)

    !parametres de cel.la
    REAL B1, B2, B3, B4, B5, B6
    !matriu de pas a cristal.lografiques
    REAL B11, B12, B13, B21, B22, B23, B31, B32, B33
    !matriu de pas a cartesianes
    REAL A11, A12, A13, A21, A22, A23, A31, A32, A33
    !constants reticulars reciproques
    REAL CR11,CR22,CR33,CR12,CR13,CR23,SK,BO,D2
    
contains

    subroutine recip()
      implicit none
      D2=1-COS(B4)**2-COS(B5)**2-COS(B6)**2+2*COS(B4)*COS(B5)*COS(B6)
      CR11 = SIN(B4)**2/(D2*B1**2)
      CR22 = SIN(B5)**2/(D2*B2**2)
      CR33 = SIN(B6)**2/(D2*B3**2)
      CR12 = (COS(B4) * COS(B5) - COS(B6)) / (B1*B2*D2)
      CR13 = (COS(B4) * COS(B6) - COS(B5)) / (B1*B3*D2)
      CR23 = (COS(B5) * COS(B6) - COS(B4)) / (B2*B3*D2)
      RETURN
    end subroutine recip

    subroutine cocrist()
      implicit none      
      REAL CA, CB, CC, SC, DIS
      CA = COS(B4)
      CB = COS(B5)
      CC = COS(B6)
      SC = SIN(B6)
      DIS=SQRT(1-CA*CA-CB*CB-CC*CC+2*CA*CB*CC)
      B11=1./B1
      B12=-CC/(SC*B1)
      B13=(CA*CC-CB)/(B1*DIS*SC)
      B21=0.
      B22=1./(B2*SC)
      B23=(CB*CC-CA)/(B2*DIS*SC)
      B31=0.
      B32=0.
      B33=SC/(B3*DIS)
      RETURN
    end subroutine cocrist
    
    subroutine cocart()
      implicit none
      REAL CA, CB, CC, SC, DIS
      CA = COS(B4)
      CB = COS(B5)
      CC = COS(B6)
      SC = SIN(B6)
      DIS=SQRT(1-CA*CA-CB*CB-CC*CC+2*CA*CB*CC)
      A11=B1
      A12=B2*CC
      A13=B3*CB
      A21=.0
      A22=B2*SC
      A23=(CA-CB*CC)*B3/SC
      A31=.0
      A32=.0
      A33=DIS*B3/SC
      RETURN
    end subroutine cocart

end module mod_Cella

