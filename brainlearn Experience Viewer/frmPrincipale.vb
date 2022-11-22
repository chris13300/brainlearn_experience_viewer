Imports System.Threading

Public Class frmPrincipale
    Public tabEPD() As String
    Public indexEPD As Integer
    Public tabPGN() As String
    Public tabStartpos() As String
    Public indexPGN As Integer
    Public tabCoups() As String
    Public indexCoup As Integer
    Public fichierINI As String
    Public fichierUCI As String
    Public fichierBIN As String
    Public tailleBIN As String
    Public chargement As Boolean
    Public tacheAnalyse As Thread
    Public coup1 As String
    Public coup2 As String
    Public listeExperience As String
    Public listePositionEPD As String
    
    Private Sub frmPrincipale_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CheckForIllegalCrossThreadCalls = False
        chargement = False
        coup1 = ""
        coup2 = ""
        profMinAnalyse = 16
        profMaxAnalyse = 32
        modeAnalyse = 1
        listePositionEPD = ""
    End Sub

    Private Sub frmPrincipale_Activated(sender As Object, e As EventArgs) Handles Me.Activated
        Dim chaine As String, tabChaine() As String, tabTmp() As String, i As Integer

        Me.Tag = Me.Text

        If Not chargement Then
            chargement = True

            ReDim tabEPD(0)
            indexEPD = 0
            tabEPD(indexEPD) = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            indexPGN = -1
            lblInfo.Text = tabEPD(indexEPD)
            fichierUCI = ""
            moteur_court = ""

            pbEchiquier.BackgroundImage = Image.FromFile("BMP\echiquier.bmp")
            pbMateriel.BackColor = Color.FromName("control")

            fichierINI = My.Computer.Name & ".ini"
            If My.Computer.FileSystem.FileExists(fichierINI) Then
                chaine = My.Computer.FileSystem.ReadAllText(fichierINI)
                If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                    tabChaine = Split(chaine, vbCrLf)
                    For i = 0 To UBound(tabChaine)
                        If tabChaine(i) <> "" And InStr(tabChaine(i), " = ") > 0 Then
                            tabTmp = Split(tabChaine(i), " = ")
                            If tabTmp(0) <> "" And tabTmp(1) <> "" Then
                                If InStr(tabTmp(1), "//") > 0 Then
                                    tabTmp(1) = Trim(gauche(tabTmp(1), tabTmp(1).IndexOf("//") - 1))
                                End If
                                Select Case tabTmp(0)
                                    Case "moteurBIN"
                                        If My.Computer.FileSystem.FileExists(tabTmp(1)) Then
                                            lblBrainLearn.Text = tabTmp(1)
                                        End If

                                    Case "profMinAnalyse"
                                        profMinAnalyse = CInt(tabTmp(1))

                                    Case "profMaxAnalyse"
                                        profMaxAnalyse = CInt(tabTmp(1))

                                    Case "modeAnalyse"
                                        modeAnalyse = CInt(tabTmp(1))

                                    Case Else

                                End Select
                            End If
                        End If
                        Application.DoEvents()
                    Next
                End If
            End If
            My.Computer.FileSystem.WriteAllText(fichierINI, "moteurBIN = " & lblBrainLearn.Text & vbCrLf _
                                                          & "profMinAnalyse = " & profMinAnalyse & vbCrLf _
                                                          & "profMaxAnalyse = " & profMaxAnalyse & vbCrLf _
                                                          & "modeAnalyse = " & modeAnalyse, False)

            fichierBIN = Replace(lblBrainLearn.Text, nomFichier(lblBrainLearn.Text), "experience.bin")

            cmdRecherche.Enabled = False
            cmdAnalyse.Enabled = False
            If My.Computer.FileSystem.FileExists(lblBrainLearn.Text) Then
                moteur_court = nomFichier(lblBrainLearn.Text)
                Me.Text = "Loading " & moteur_court & "..."
                If InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                    lblMoteur.Text = "BrainLearn :"
                ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                    lblMoteur.Text = "JudaS :"
                End If
                chargerMoteur(lblBrainLearn.Text)
                tailleBIN = 0
                If My.Computer.FileSystem.FileExists(fichierBIN) Then
                    tailleBIN = FileLen(fichierBIN)
                End If
                lblInfo.Text = "experience.bin : " & Trim(Format(tailleBIN / 24, "### ### ##0 moves"))

                cmdRecherche.Enabled = True
                cmdAnalyse.Enabled = True
            End If

            If indexEPD >= 0 And moteur_court <> "" Then
                If tabEPD(indexEPD) <> "" Then
                    afficherBIN(tabEPD(indexEPD))
                End If
            End If
        End If

        Me.Text = Me.Tag
    End Sub

    Private Sub frmPrincipale_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        If lblBrainLearn.Text <> "" Then
            dechargerMoteur()
            If My.Computer.FileSystem.FileExists(fichierUCI) Then
                My.Computer.FileSystem.DeleteFile(fichierUCI)
            End If
            Do
                Threading.Thread.Sleep(1000)
            Loop While My.Computer.FileSystem.FileExists(Replace(fichierBIN, "experience.bin", "experience_new.bin"))
        End If
    End Sub

    Private Sub pbEchiquier_Paint(sender As Object, e As PaintEventArgs) Handles pbEchiquier.Paint
        If indexEPD >= 0 And moteur_court <> "" Then
            Try
                echiquier(tabEPD(indexEPD), e, coup1, coup2)
                pbMateriel.Refresh()
            Catch ex As Exception

            End Try
        End If
    End Sub

    Private Sub afficherBIN(positionEPD As String)
        Dim chaine As String, tabChaine() As String, tabTmp() As String, i As Integer
        Dim keyUCI As String, keyBIN As String

        keyUCI = uciKEY(entree, sortie, "", positionEPD)

        keyBIN = ""
        For j = 0 To 15 Step 2
            chaine = keyUCI.Substring(j, 2)
            If Len(chaine) = 1 Then
                chaine = "0" & chaine
            End If
            keyBIN = chaine & keyBIN
        Next
        keyUCI = keyBIN
        keyBIN = ""

afficherbin:
        Try
            If InStr(listePositionEPD, positionEPD & "|", CompareMethod.Text) = 0 Then
                listeExperience = brainlearn_expListe(fichierBIN, keyUCI)
                listePositionEPD = listePositionEPD & positionEPD & "|" & listeExperience & "|"
            Else
                listeExperience = listePositionEPD.Substring(listePositionEPD.IndexOf(positionEPD))
                listeExperience = Replace(listeExperience, positionEPD & "|", "")
                listeExperience = listeExperience.Substring(0, listeExperience.IndexOf("|"))
            End If
        Catch ex As Exception
            GoTo afficherbin
        End Try

        If listeExperience = "" Then
            For i = 1 To 20
                Me.Controls("lblRang" & i).Text = Format(i, "00")
                Me.Controls("lblCoup" & i).Text = ""
                Me.Controls("lblScore" & i).Text = ""
                Me.Controls("lblScore" & i).ForeColor = Color.Black
                Me.Controls("lblQualite" & i).Text = ""
                Application.DoEvents()
            Next
            Exit Sub
        End If
        tabChaine = Split(listeExperience, vbCrLf)
        For i = 1 To 20
            Me.Controls("lblRang" & i).Text = Format(i, "00")
            If i < tabChaine.Length Then
                tabTmp = Split(Replace(tabChaine(i - 1), ":", ","), ",")

                Me.Controls("lblCoup" & i).Text = Trim(tabTmp(1))

                If InStr(tabTmp(5), "cp") > 0 Then
                    'white pov
                    If InStr(positionEPD, " w ") > 0 Then
                        tabTmp(5) = Format(Val(Replace(tabTmp(5), "cp ", "")) / 100, "0.00")
                    ElseIf InStr(positionEPD, " b ") > 0 Then
                        tabTmp(5) = Format(-Val(Replace(tabTmp(5), "cp ", "")) / 100, "0.00")
                    End If
                    If CSng(tabTmp(5)) > 0 Then
                        tabTmp(5) = "+" & tabTmp(5)
                    End If
                ElseIf InStr(tabTmp(5), "mate") > 0 Then
                    'white pov
                    If InStr(positionEPD, " w ") > 0 Then
                        tabTmp(5) = Replace(tabTmp(5), "mate ", "")
                    ElseIf InStr(positionEPD, " b ") > 0 Then
                        tabTmp(5) = Format(-CInt(Replace(tabTmp(5), "mate ", "")))
                    End If
                    If CInt(tabTmp(5)) > 0 Then
                        tabTmp(5) = "+M" & Trim(tabTmp(5))
                    ElseIf CInt(tabTmp(5)) < 0 Then
                        tabTmp(5) = "-M" & Trim(Replace(tabTmp(5), "-", ""))
                    End If
                End If
                If InStr(tabTmp(5), "+") > 0 Then
                    If InStr(positionEPD, " w ", CompareMethod.Text) > 0 Then
                        Me.Controls("lblScore" & i).ForeColor = Color.Blue
                    ElseIf InStr(positionEPD, " b ", CompareMethod.Text) > 0 Then
                        Me.Controls("lblScore" & i).ForeColor = Color.Red
                    End If
                ElseIf InStr(tabTmp(5), "-") > 0 Then
                    If InStr(positionEPD, " w ", CompareMethod.Text) > 0 Then
                        Me.Controls("lblScore" & i).ForeColor = Color.Red
                    ElseIf InStr(positionEPD, " b ", CompareMethod.Text) > 0 Then
                        Me.Controls("lblScore" & i).ForeColor = Color.Blue
                    End If
                Else
                    Me.Controls("lblScore" & i).ForeColor = Color.Black
                End If
                Me.Controls("lblScore" & i).Text = "{" & tabTmp(5) & "/" & Trim(tabTmp(3)) & "}"

                Me.Controls("lblQualite" & i).Text = Trim(tabTmp(7)) & "%"
            Else
                Me.Controls("lblCoup" & i).Text = ""
                Me.Controls("lblScore" & i).Text = ""
                Me.Controls("lblScore" & i).ForeColor = Color.Black
                Me.Controls("lblQualite" & i).Text = ""
            End If
            Application.DoEvents()
        Next
    End Sub

    Private Sub pbMateriel_Paint(sender As Object, e As PaintEventArgs) Handles pbMateriel.Paint
        If indexEPD >= 0 And moteur_court <> "" Then
            echiquier_differences(tabEPD(indexEPD), e)
        End If
    End Sub

    Private Sub cmdMoteur_Click(sender As Object, e As EventArgs) Handles cmdMoteur.Click
        Me.Tag = Me.Text

        dlgImport.FileName = ""
        dlgImport.InitialDirectory = Environment.CurrentDirectory
        dlgImport.Filter = "Moteur EXE (*.exe)|*.exe"
        dlgImport.ShowDialog()
        If dlgImport.FileName = "" Then
            Exit Sub
        End If

        lblBrainLearn.Text = dlgImport.FileName
        My.Computer.FileSystem.WriteAllText(fichierINI, "moteurBIN = " & lblBrainLearn.Text & vbCrLf _
                                                      & "profMinAnalyse = " & profMinAnalyse & vbCrLf _
                                                      & "profMaxAnalyse = " & profMaxAnalyse & vbCrLf _
                                                      & "modeAnalyse = " & modeAnalyse, False)

        fichierBIN = Replace(lblBrainLearn.Text, nomFichier(lblBrainLearn.Text), "experience.bin")

        dechargerMoteur()
        Do
            Threading.Thread.Sleep(1000)
        Loop While My.Computer.FileSystem.FileExists(Replace(fichierBIN, "experience.bin", "experience_new.bin"))

        cmdRecherche.Enabled = False
        cmdAnalyse.Enabled = False
        If My.Computer.FileSystem.FileExists(lblBrainLearn.Text) Then
            moteur_court = nomFichier(lblBrainLearn.Text)
            Me.Text = "Loading " & moteur_court & "..."
            If InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                lblMoteur.Text = "BrainLearn :"
            ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                lblMoteur.Text = "JudaS :"
            End If
            chargerMoteur(lblBrainLearn.Text)
            tailleBIN = 0
            If My.Computer.FileSystem.FileExists(fichierBIN) Then
                tailleBIN = FileLen(fichierBIN)
            End If
            lblInfo.Text = "experience.bin : " & Trim(Format(tailleBIN / 24, "### ### ##0 moves"))

            cmdRecherche.Enabled = True
            cmdAnalyse.Enabled = True
        End If

        If indexEPD >= 0 And moteur_court <> "" Then
            If tabEPD(indexEPD) <> "" Then
                afficherBIN(tabEPD(indexEPD))
            End If
        End If

        Me.Text = Me.Tag
    End Sub

    Private Sub cmdRecherche_Click(sender As Object, e As EventArgs) Handles cmdRecherche.Click
        Dim chaineUCI As String, chaineStartpos As String, blanc As String, noir As String, resultat As String, eco As String, startpos As String

        If txtRecherche.Text <> "" Then
            If My.Computer.FileSystem.FileExists(fichierUCI) Then
                My.Computer.FileSystem.DeleteFile(fichierUCI)
                lstParties.Items.Clear()
            End If
        End If

        dlgImport.FileName = ""
        dlgImport.InitialDirectory = Environment.CurrentDirectory
        dlgImport.Filter = "Fichier PGN (*.pgn)|*.pgn|Fichier EPD (*.epd)|*.epd"
        dlgImport.ShowDialog()
        If dlgImport.FileName = "" Then
            Exit Sub
        End If

        txtRecherche.Text = dlgImport.FileName
        If extensionFichier(txtRecherche.Text) = ".pgn" Then
            cmdLignePrecedente.Text = "<<"
            cmdLignePrecedente.Visible = True
            cmdCoupPrecedent.Visible = True
            cmdCoupSuivant.Visible = True
            cmdLigneSuivante.Text = ">>"
            cmdLigneSuivante.Visible = True
            lblRecherche.Text = "Mode PGN :"

            fichierUCI = Replace(txtRecherche.Text, ".pgn", "_uci.pgn")
            If My.Computer.FileSystem.FileExists(fichierUCI) Then
                My.Computer.FileSystem.DeleteFile(fichierUCI)
            End If
            pgnUCI("pgn-extract.exe", txtRecherche.Text, "_uci")
            tabPGN = Split(My.Computer.FileSystem.ReadAllText(fichierUCI), vbCrLf)
            chaineUCI = ""
            chaineStartpos = ""
            blanc = ""
            noir = ""
            resultat = ""
            eco = "???"
            startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            Me.Width = 1280
            lstParties.Visible = True
            Me.CenterToScreen()
            For i = 0 To UBound(tabPGN)
                If tabPGN(i) <> "" Then
                    If InStr(tabPGN(i), "[White ") > 0 Then
                        blanc = Replace(Replace(tabPGN(i), "[White """, ""), """]", "")
                    ElseIf InStr(tabPGN(i), "[Black ") > 0 Then
                        noir = Replace(Replace(tabPGN(i), "[Black """, ""), """]", "")
                    ElseIf InStr(tabPGN(i), "[Result ") > 0 Then
                        resultat = Replace(Replace(tabPGN(i), "[Result """, ""), """]", "")
                    ElseIf InStr(tabPGN(i), "[ECO ") > 0 Then
                        eco = Replace(Replace(tabPGN(i), "[ECO """, ""), """]", "")
                    ElseIf InStr(tabPGN(i), "[FEN ") > 0 Then
                        startpos = Replace(Replace(tabPGN(i), "[FEN """, ""), """]", "")
                    ElseIf InStr(tabPGN(i), "[") = 0 And InStr(tabPGN(i), "]") = 0 Then
                        tabPGN(i) = Trim(Replace(tabPGN(i), "*", ""))
                        tabPGN(i) = Trim(Replace(tabPGN(i), "1-0", ""))
                        tabPGN(i) = Trim(Replace(tabPGN(i), "0-1", ""))
                        tabPGN(i) = Trim(Replace(tabPGN(i), "1/2-1/2", ""))
                        lstParties.Items.Add(Format(lstParties.Items.Count + 1, "000") & " (" & eco & ", " & Format(tabPGN(i).Split(" ").Length, "000 plies") & ") : " & blanc & " - " & noir & " (" & Replace(resultat, "1/2-1/2", "=-=") & ") " & tabPGN(i))
                        chaineUCI = chaineUCI & tabPGN(i) & vbCrLf
                        chaineStartpos = chaineStartpos & startpos & vbCrLf
                        blanc = ""
                        noir = ""
                        resultat = ""
                        eco = "???"
                        startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                    End If
                End If
                Application.DoEvents()
            Next
            tabPGN = Split(chaineUCI, vbCrLf)
            tabStartpos = Split(chaineStartpos, vbCrLf)
            indexPGN = -1
        ElseIf extensionFichier(txtRecherche.Text) = ".epd" Then
            cmdLignePrecedente.Text = "<"
            cmdLignePrecedente.Visible = True
            cmdCoupPrecedent.Visible = False
            cmdCoupSuivant.Visible = False
            cmdLigneSuivante.Text = ">"
            cmdLigneSuivante.Visible = True
            lblRecherche.Text = "Mode EPD :"
            tabEPD = Split(My.Computer.FileSystem.ReadAllText(txtRecherche.Text), vbCrLf)
            Me.Width = 1280
            lstParties.Visible = True
            Me.CenterToScreen()
            For i = 0 To UBound(tabEPD)
                If tabEPD(i) <> "" Then
                    lstParties.Items.Add(Format(lstParties.Items.Count + 1, "000") & " : " & tabEPD(i))
                End If
                Application.DoEvents()
            Next
            indexEPD = -1
        End If
        cmdLigneSuivante_Click(sender, e)
    End Sub

    Private Sub cmdLignePrecedente_Click(sender As Object, e As EventArgs) Handles cmdLignePrecedente.Click
        Dim chaine As String

        If fichierUCI = "" Then
            If 0 < indexEPD Then
                indexEPD = indexEPD - 1
                lblInfo.Text = tabEPD(indexEPD)
                coup1 = ""
                pbEchiquier.Refresh()
                If indexEPD >= 0 And moteur_court <> "" Then
                    If tabEPD(indexEPD) <> "" Then
                        afficherBIN(tabEPD(indexEPD))
                    End If
                End If
            End If
        Else
            'partie precedente
            If 0 < indexPGN Then
                indexPGN = indexPGN - 1
                lstParties.SetSelected(indexPGN, True)
                If tabPGN(indexPGN) <> "" Then
                    tabCoups = Split(tabPGN(indexPGN), " ")
                    indexCoup = 0
                    If tabCoups(indexCoup) <> "" Then
                        chaine = tabCoups(indexCoup)
                        lblInfo.Text = "PGN " & (indexPGN + 1) & "/" & UBound(tabPGN) & " : " & chaine
                        ReDim tabEPD(0)
                        indexEPD = 0
                        tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, chaine, tabStartpos(indexPGN))
                        coup1 = ""
                        If indexCoup < UBound(tabCoups) Then
                            coup1 = tabCoups(indexCoup + 1)
                        End If
                        pbEchiquier.Refresh()
                        If indexEPD >= 0 And moteur_court <> "" Then
                            If tabEPD(indexEPD) <> "" Then
                                afficherBIN(tabEPD(indexEPD))
                            End If
                        End If
                    End If
                End If
            End If
        End If

    End Sub

    Private Sub cmdCoupPrecedent_Click(sender As Object, e As EventArgs) Handles cmdCoupPrecedent.Click
        Dim chaine As String

        If 0 < indexCoup Then
            indexCoup = indexCoup - 1
            If fichierUCI = "" Then
                indexEPD = indexEPD - 1
                If tabCoups(indexCoup) <> "" Then
                    lstParties.SetSelected(indexEPD, True)
                    lblInfo.Text = lblInfo.Tag
                    If InStr(lblInfo.Tag, " ") > 0 Then
                        lblInfo.Tag.substring(0, lblInfo.Tag.indexof(" "))
                    End If
                    coup1 = ""
                    pbEchiquier.Refresh()
                    If indexEPD >= 0 And moteur_court <> "" Then
                        If tabEPD(indexEPD) <> "" Then
                            afficherBIN(tabEPD(indexEPD))
                        End If
                    End If
                End If
            Else
                If tabCoups(indexCoup) <> "" Then
                    chaine = lblInfo.Tag
                    lblInfo.Text = "PGN " & (indexPGN + 1) & "/" & UBound(tabPGN) & " : " & chaine
                    If InStr(lblInfo.Tag, " ") > 0 Then
                        lblInfo.Tag = chaine.Substring(0, chaine.LastIndexOf(" "))
                    End If
                    ReDim tabEPD(0)
                    indexEPD = 0
                    tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, chaine, tabStartpos(indexPGN))
                    coup1 = ""
                    If indexCoup < UBound(tabCoups) Then
                        coup1 = tabCoups(indexCoup + 1)
                    End If
                    pbEchiquier.Refresh()
                    If indexEPD >= 0 And moteur_court <> "" Then
                        If tabEPD(indexEPD) <> "" Then
                            afficherBIN(tabEPD(indexEPD))
                        End If
                    End If
                End If
            End If

        End If
    End Sub

    Private Sub cmdCoupSuivant_Click(sender As Object, e As EventArgs) Handles cmdCoupSuivant.Click
        Dim chaine As String

        If indexCoup < UBound(tabCoups) Then
            indexCoup = indexCoup + 1
            If fichierUCI = "" Then
                indexEPD = indexEPD + 1
                If tabCoups(indexCoup) <> "" Then
                    lblInfo.Tag = lblInfo.Text
                    lblInfo.Text = Trim(lblInfo.Text & " " & tabCoups(indexCoup))
                    coup1 = ""
                    pbEchiquier.Refresh()
                    If indexEPD >= 0 And moteur_court <> "" Then
                        If tabEPD(indexEPD) <> "" Then
                            afficherBIN(tabEPD(indexEPD))
                        End If
                    End If
                End If
            Else
                If tabCoups(indexCoup) <> "" Then
                    lblInfo.Tag = lblInfo.Text.Substring(lblInfo.Text.IndexOf(":") + 2)
                    chaine = Trim(lblInfo.Tag & " " & tabCoups(indexCoup))
                    lblInfo.Text = "PGN " & (indexPGN + 1) & "/" & UBound(tabPGN) & " : " & chaine
                    ReDim tabEPD(0)
                    indexEPD = 0
                    tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, chaine, tabStartpos(indexPGN))
                    coup1 = ""
                    If indexCoup < UBound(tabCoups) Then
                        coup1 = tabCoups(indexCoup + 1)
                    End If
                    pbEchiquier.Refresh()
                    If indexEPD >= 0 And moteur_court <> "" Then
                        If tabEPD(indexEPD) <> "" Then
                            afficherBIN(tabEPD(indexEPD))
                        End If
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub cmdLigneSuivante_Click(sender As Object, e As EventArgs) Handles cmdLigneSuivante.Click
        Dim chaine As String

        If fichierUCI = "" Then
            If indexEPD < UBound(tabEPD) Then
                indexEPD = indexEPD + 1
                If tabEPD(indexEPD) <> "" Then
                    lstParties.SetSelected(indexEPD, True)
                    lblInfo.Text = tabEPD(indexEPD)
                    coup1 = ""
                    pbEchiquier.Refresh()
                    If indexEPD >= 0 And moteur_court <> "" Then
                        If tabEPD(indexEPD) <> "" Then
                            afficherBIN(tabEPD(indexEPD))
                        End If
                    End If
                End If
            End If
        Else
            'partie suivante
            If indexPGN < UBound(tabPGN) Then
                indexPGN = indexPGN + 1
                If tabPGN(indexPGN) <> "" Then
                    lstParties.SetSelected(indexPGN, True)
                    tabCoups = Split(tabPGN(indexPGN), " ")
                    indexCoup = 0
                    If tabCoups(indexCoup) <> "" Then
                        chaine = tabCoups(indexCoup)
                        lblInfo.Text = "PGN " & (indexPGN + 1) & "/" & UBound(tabPGN) & " : " & chaine
                        ReDim tabEPD(0)
                        indexEPD = 0
                        tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, chaine, tabStartpos(indexPGN))
                        coup1 = ""
                        If indexCoup < UBound(tabCoups) Then
                            coup1 = tabCoups(indexCoup + 1)
                        End If
                        pbEchiquier.Refresh()
                        If indexEPD >= 0 And moteur_court <> "" Then
                            If tabEPD(indexEPD) <> "" Then
                                afficherBIN(tabEPD(indexEPD))
                            End If
                        End If
                    End If
                End If
            End If
        End If

    End Sub

    Private Sub txtRecherche_KeyUp(sender As Object, e As KeyEventArgs) Handles txtRecherche.KeyUp
        Dim tabTmp() As String
        If e.KeyCode = Keys.Enter Then
            Cursor = Cursors.WaitCursor
            If nbCaracteres(txtRecherche.Text, "/") = 7 _
            And (InStr(txtRecherche.Text, " w ") > 0 Or InStr(txtRecherche.Text, " b ") > 0) Then
                lblRecherche.Text = "Position EPD :"
                ReDim tabEPD(0)
                indexEPD = 0
                If InStr(txtRecherche.Text, " moves ", CompareMethod.Text) > 0 Then
                    tabTmp = Split(txtRecherche.Text, " moves ")
                    tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, tabTmp(1), tabTmp(0))
                Else
                    tabEPD(indexEPD) = txtRecherche.Text
                End If
                lblInfo.Text = tabEPD(indexEPD)
                coup1 = ""
                pbEchiquier.Refresh()
                If indexEPD >= 0 And moteur_court <> "" Then
                    If tabEPD(indexEPD) <> "" Then
                        afficherBIN(tabEPD(indexEPD))
                    End If
                End If
                If Me.Width <> 626 Then
                    Me.Width = 626
                    lstParties.Visible = False
                    Me.CenterToScreen()
                End If
            Else
                lblRecherche.Text = "Suite UCI :"
                ReDim tabEPD(0)
                indexEPD = 0
                tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, txtRecherche.Text)
                lblInfo.Text = tabEPD(indexEPD)
                coup1 = ""
                pbEchiquier.Refresh()
                If indexEPD >= 0 And moteur_court <> "" Then
                    If tabEPD(indexEPD) <> "" Then
                        afficherBIN(tabEPD(indexEPD))
                    End If
                End If
                If Me.Width <> 626 Then
                    Me.Width = 626
                    lstParties.Visible = False
                    Me.CenterToScreen()
                End If
            End If
            cmdLignePrecedente.Visible = False
            cmdCoupPrecedent.Visible = False
            cmdCoupSuivant.Visible = False
            cmdLigneSuivante.Visible = False
            Cursor = Cursors.Default
        End If
    End Sub

    Private Sub lstParties_DoubleClick(sender As Object, e As EventArgs) Handles lstParties.DoubleClick
        Dim chaine As String

        If fichierUCI = "" Then
            indexEPD = lstParties.SelectedIndex
            lblInfo.Text = tabEPD(indexEPD)
            pbEchiquier.Refresh()
            If indexEPD >= 0 And moteur_court <> "" Then
                If tabEPD(indexEPD) <> "" Then
                    afficherBIN(tabEPD(indexEPD))
                End If
            End If
        Else
            indexPGN = lstParties.SelectedIndex
            If tabPGN(indexPGN) <> "" Then
                tabCoups = Split(tabPGN(indexPGN), " ")
                indexCoup = 0
                If tabCoups(indexCoup) <> "" Then
                    chaine = tabCoups(indexCoup)
                    lblInfo.Text = "PGN " & (indexPGN + 1) & "/" & UBound(tabPGN) & " : " & chaine
                    ReDim tabEPD(0)
                    indexEPD = 0
                    tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, chaine, tabStartpos(indexPGN))
                    pbEchiquier.Refresh()
                    If indexEPD >= 0 And moteur_court <> "" Then
                        If tabEPD(indexEPD) <> "" Then
                            afficherBIN(tabEPD(indexEPD))
                        End If
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub cmdAnalyse_Click(sender As Object, e As EventArgs) Handles cmdAnalyse.Click
        analyse = False
        frmOptions.ShowDialog()
        If Not analyse Then
            Exit Sub
        End If

        My.Computer.FileSystem.WriteAllText(fichierINI, "moteurBIN = " & lblBrainLearn.Text & vbCrLf _
                                                      & "profMinAnalyse = " & profMinAnalyse & vbCrLf _
                                                      & "profMaxAnalyse = " & profMaxAnalyse & vbCrLf _
                                                      & "modeAnalyse = " & modeAnalyse, False)

        cmdAnalyse.Visible = False
        cmdDefrag.Visible = False
        cmdStop.Visible = True
        txtRecherche.Enabled = False
        cmdMoteur.Enabled = False
        cmdRecherche.Enabled = False
        lstParties.Enabled = False

        timerAnalyse.Enabled = True
        tacheAnalyse = New Thread(AddressOf analysePosition)
        tacheAnalyse.Start()

    End Sub

    Private Sub timerAnalyse_Tick(sender As Object, e As EventArgs) Handles timerAnalyse.Tick
        If Not cmdStop.Visible Then
            timerAnalyse.Enabled = False
            coup1 = ""
            pbEchiquier.Refresh()

            cmdAnalyse.Visible = True
            cmdDefrag.Visible = True
            txtRecherche.Enabled = True
            cmdMoteur.Enabled = True
            cmdRecherche.Enabled = True
            lstParties.Enabled = True

            Me.Text = My.Application.Info.AssemblyName
            Application.DoEvents()
        End If

    End Sub

    Private Function dejaAnalysee(coup As String, profDemandee As Integer) As Boolean
        Dim i As Integer, trouve As Boolean, tabChaine() As String, prof As Integer
        Dim tabTmp() As String

        trouve = False
        tabChaine = Split(listeExperience, vbCrLf)
        For i = 0 To UBound(tabChaine)
            If tabChaine(i) <> "" Then
                tabTmp = Split(Replace(tabChaine(i), ":", ","), ",")
                If coup = Trim(tabTmp(1)) Then
                    prof = CInt(Trim(tabTmp(3)))
                    If prof >= profDemandee Then
                        trouve = True
                        Exit For
                    End If
                End If
            End If
        Next

        Return trouve
    End Function

    Private Sub analysePosition()
        Dim chaine As String, chaineInfo As String, liste As String, tabChaine() As String, prof As Integer, tabTmp() As String

        If cmdStop.Visible Then
            entree.WriteLine("position fen " & tabEPD(indexEPD))

            liste = ""
            Select Case modeAnalyse
                Case 1
                    'on cherche tous les coups possibles
                    entree.WriteLine("setoption name MultiPV value " & maxMultiPVMoteur(moteur_court))
                    entree.WriteLine("go depth 1")

                    chaine = ""
                    While InStr(chaine, "bestmove", CompareMethod.Text) = 0
                        chaine = sortie.ReadLine
                        If InStr(chaine, " pv ", CompareMethod.Text) > 0 Then
                            tabChaine = Split(chaine, " ")
                            For i = 0 To UBound(tabChaine) - 1
                                If InStr(tabChaine(i), "pv", CompareMethod.Text) > 0 _
                                And tabChaine(i + 1) <> "" And Len(tabChaine(i + 1)) = 4 Then
                                    liste = liste & tabChaine(i + 1) & vbCrLf
                                End If
                            Next
                        End If
                        Threading.Thread.Sleep(1)
                    End While
                    entree.WriteLine("stop")

                    entree.WriteLine("isready")
                    chaine = ""
                    While InStr(chaine, "readyok") = 0
                        chaine = sortie.ReadLine
                        Threading.Thread.Sleep(1)
                    End While

                Case 2 'only played moves
                    tabChaine = Split(listeExperience, vbCrLf)
                    For i = 1 To 20
                        If i < tabChaine.Length Then
                            If tabChaine(i - 1) <> "" Then
                                tabTmp = Split(Replace(tabChaine(i - 1), ":", ","), ",")
                                liste = liste & Trim(tabTmp(1)) & vbCrLf
                            End If
                        End If
                    Next

                Case 3 'only moves with positive score
                    tabChaine = Split(listeExperience, vbCrLf)
                    For i = 1 To 20
                        If i < tabChaine.Length Then
                            If tabChaine(i - 1) <> "" Then
                                tabTmp = Split(Replace(tabChaine(i - 1), ":", ","), ",")
                                If InStr(tabTmp(5), "-") = 0 Then
                                    liste = liste & Trim(tabTmp(1)) & vbCrLf
                                End If
                            End If
                        End If
                    Next

                Case 4 'search bestmove
                    liste = liste & vbCrLf

                Case Else

            End Select

            'on analyse les coups les uns après les autres
            entree.WriteLine("setoption name MultiPV value 1")
            tabChaine = Split(trierChaine(liste, vbCrLf), vbCrLf)
            prof = profMinAnalyse
            chaineInfo = ""
            While cmdStop.Visible And prof <= profMaxAnalyse
                For i = 0 To UBound(tabChaine)
                    If (modeAnalyse = 4 Or tabChaine(i) <> "") And Not dejaAnalysee(tabChaine(i), prof) Then
                        If modeAnalyse = 4 Then
                            entree.WriteLine("go depth " & prof)
                            If chaineInfo <> "" Then
                                coup1 = Trim(chaineInfo.Substring(0, chaineInfo.IndexOf(" ")))
                            End If
                        Else
                            entree.WriteLine("go depth " & prof & " searchmoves " & tabChaine(i))
                            coup1 = tabChaine(i)
                        End If

                        pbEchiquier.Refresh()

                        chaine = ""
                        chaineInfo = ""
                        While cmdStop.Visible And InStr(chaine, "bestmove", CompareMethod.Text) = 0
                            chaine = sortie.ReadLine
                            If InStr(chaine, " pv ", CompareMethod.Text) > 0 And InStr(chaine, "upperbound", CompareMethod.Text) = 0 And InStr(chaine, "lowerbound", CompareMethod.Text) = 0 And InStr(chaine, " depth " & prof, CompareMethod.Text) > 0 Then
                                chaineInfo = formaterInfos(chaine, tabEPD(indexEPD))
                                If InStr(chaineInfo, "mate", CompareMethod.Text) > 0 Then
                                    chaineInfo = Replace(chaineInfo, "{mate -", "{-M")
                                    chaineInfo = Replace(chaineInfo, "{mate ", "{+M")
                                End If
                                Me.Text = "Prof " & Format(prof) & "/" & profMaxAnalyse & ", coup " & Format(i + 1) & "/" & tabChaine.Length & " : " & chaineInfo
                            End If
                            Threading.Thread.Sleep(1)
                        End While
                        entree.WriteLine("stop")

                        entree.WriteLine("isready")
                        chaine = ""
                        While InStr(chaine, "readyok") = 0
                            chaine = sortie.ReadLine
                            Threading.Thread.Sleep(1)
                        End While
                    End If
                Next
                entree.WriteLine("ucinewgame")

                entree.WriteLine("isready")
                chaine = ""
                While InStr(chaine, "readyok") = 0
                    chaine = sortie.ReadLine
                    Threading.Thread.Sleep(1)
                End While

                prof = prof + 2

                chaine = listePositionEPD.Substring(listePositionEPD.IndexOf(tabEPD(indexEPD)))
                If chaine = tabEPD(indexEPD) & "||" Then
                    listePositionEPD = Replace(listePositionEPD, chaine, "")
                Else
                    chaine = chaine.Substring(chaine.IndexOf("|") + 1)
                    chaine = tabEPD(indexEPD) & "|" & chaine.Substring(0, chaine.IndexOf("|") + 1)
                    listePositionEPD = Replace(listePositionEPD, chaine, "")
                End If

                afficherBIN(tabEPD(indexEPD))
            End While
            coup1 = ""

            cmdStop_Click(Nothing, Nothing)

            pbEchiquier.Refresh()
            If indexEPD >= 0 And moteur_court <> "" Then
                If tabEPD(indexEPD) <> "" Then
                    afficherBIN(tabEPD(indexEPD))
                End If
            End If
        End If
    End Sub

    Private Sub cmdStop_Click(sender As Object, e As EventArgs) Handles cmdStop.Click
        Dim chaine As String

        entree.WriteLine("stop")

        entree.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While

        cmdStop.Visible = False
    End Sub

    Private Sub cmdDefrag_Click(sender As Object, e As EventArgs) Handles cmdDefrag.Click
        Me.Tag = Me.Text

        cmdAnalyse.Enabled = False
        cmdDefrag.Enabled = False
        txtRecherche.Enabled = False
        cmdMoteur.Enabled = False
        cmdRecherche.Enabled = False

        Me.Text = "Defrag experience.bin..."

        'il faut d'abord arrêter le moteur car il risque d'écrire dans le fichier experience.bin
        entree.Close()
        sortie.Close()
        processus.Close()

        Do
            Threading.Thread.Sleep(1000)
        Loop While My.Computer.FileSystem.FileExists(Replace(fichierBIN, "experience.bin", "experience_new.bin"))

        'ensuite on défragmente
        listePositionEPD = ""
        lblInfo.Text = defragBIN(fichierBIN, 1)
        Application.DoEvents()

        'ensuite on recharge moteur qui rechargera le nouveau fichier experience.bin
        Me.Text = "Loading " & moteur_court & "..."
        chargerMoteur(lblBrainLearn.Text)
        tailleBIN = FileLen(fichierBIN)
        
        pbEchiquier.Refresh()
        If indexEPD >= 0 And moteur_court <> "" Then
            If tabEPD(indexEPD) <> "" Then
                afficherBIN(tabEPD(indexEPD))
            End If
        End If

        cmdAnalyse.Enabled = True
        cmdDefrag.Enabled = True
        txtRecherche.Enabled = True
        cmdMoteur.Enabled = True
        cmdRecherche.Enabled = True

        Me.Text = Me.Tag
        Application.DoEvents()
    End Sub

    Private Sub lblRang1_DoubleClick(sender As Object, e As EventArgs) Handles lblRang1.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup1.Text, lblScore1.Text)
    End Sub

    Private Sub lblRang2_DoubleClick(sender As Object, e As EventArgs) Handles lblRang2.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup2.Text, lblScore2.Text)
    End Sub

    Private Sub lblRang3_DoubleClick(sender As Object, e As EventArgs) Handles lblRang3.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup3.Text, lblScore3.Text)
    End Sub

    Private Sub lblRang4_DoubleClick(sender As Object, e As EventArgs) Handles lblRang4.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup4.Text, lblScore4.Text)
    End Sub

    Private Sub lblRang5_DoubleClick(sender As Object, e As EventArgs) Handles lblRang5.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup5.Text, lblScore5.Text)
    End Sub

    Private Sub lblRang6_DoubleClick(sender As Object, e As EventArgs) Handles lblRang6.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup6.Text, lblScore6.Text)
    End Sub

    Private Sub lblRang7_DoubleClick(sender As Object, e As EventArgs) Handles lblRang7.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup7.Text, lblScore7.Text)
    End Sub

    Private Sub lblRang8_DoubleClick(sender As Object, e As EventArgs) Handles lblRang8.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup8.Text, lblScore8.Text)
    End Sub

    Private Sub lblRang9_DoubleClick(sender As Object, e As EventArgs) Handles lblRang9.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup9.Text, lblScore9.Text)
    End Sub

    Private Sub lblRang10_DoubleClick(sender As Object, e As EventArgs) Handles lblRang10.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup10.Text, lblScore10.Text)
    End Sub

    Private Sub lblRang11_DoubleClick(sender As Object, e As EventArgs) Handles lblRang11.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup11.Text, lblScore11.Text)
    End Sub

    Private Sub lblRang12_DoubleClick(sender As Object, e As EventArgs) Handles lblRang12.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup12.Text, lblScore12.Text)
    End Sub

    Private Sub lblRang13_DoubleClick(sender As Object, e As EventArgs) Handles lblRang13.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup13.Text, lblScore13.Text)
    End Sub

    Private Sub lblRang14_DoubleClick(sender As Object, e As EventArgs) Handles lblRang14.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup14.Text, lblScore14.Text)
    End Sub

    Private Sub lblRang15_DoubleClick(sender As Object, e As EventArgs) Handles lblRang15.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup15.Text, lblScore15.Text)
    End Sub

    Private Sub lblRang16_DoubleClick(sender As Object, e As EventArgs) Handles lblRang16.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup16.Text, lblScore16.Text)
    End Sub

    Private Sub lblRang17_DoubleClick(sender As Object, e As EventArgs) Handles lblRang17.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup17.Text, lblScore17.Text)
    End Sub

    Private Sub lblRang18_DoubleClick(sender As Object, e As EventArgs) Handles lblRang18.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup18.Text, lblScore18.Text)
    End Sub

    Private Sub lblRang19_DoubleClick(sender As Object, e As EventArgs) Handles lblRang19.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup19.Text, lblScore19.Text)
    End Sub

    Private Sub lblRang20_DoubleClick(sender As Object, e As EventArgs) Handles lblRang20.DoubleClick
        supCoup(entree, sortie, tabEPD(indexEPD), lblCoup20.Text, lblScore20.Text)
    End Sub

    Private Sub supCoup(entreeRang As System.IO.StreamWriter, sortieRang As System.IO.StreamReader, epd As String, coup As String, scoreDepth As String)
        Dim critereBIN(23) As Byte, critereBINbis(23) As Byte, profActuelle As Integer
        Dim lectureBIN As IO.FileStream, tabBIN(23) As Byte, posLecture As Long
        Dim tabBINtmp(23) As Byte, reponse As String

        If coup = "" Then
            Exit Sub
        End If
        Cursor = Cursors.WaitCursor
        txtRecherche.Enabled = False

        profActuelle = CInt(Replace(scoreDepth.Substring(scoreDepth.IndexOf("/") + 1), "}", ""))

        'position               | Depth |  ?  ?   ?  | score           | move    | ?   ?   | Performance | ?   ?   ?
        '0 1  2  3  4  5  6  7  | 8     |  9  A   B  | C   D   E   F   | 0   1   | 2   3   | 4           | 5   6   7 (24 octets)
        '0 +1 +2 +3 +4 +5 +6 +7 | +8    | +9 +10 +11 | +12 +13 +14 +15 | +16 +17 | +18 +19 | +20         | +21 +22 +23 tabBIN
        'tabBIN(i+8) = 8 (profondeur)

        If MsgBox("Delete '" & coup & "' ?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
            'on prépare un critère de recherche basé sur la position en cours et le coup sélectionné
            Array.Copy(epdToEXP(entreeRang, sortieRang, epd), 0, critereBIN, 0, 8) '0-7
            Array.Copy(moveToBIN(coup), 0, critereBIN, 16, 2) '0-1
            Array.Copy(moveToBIN(coup, True), 0, critereBINbis, 16, 2) '0-1

            'on efface cette position de la liste
            reponse = listePositionEPD.Substring(listePositionEPD.IndexOf(epd))
            If reponse = epd & "||" Then
                listePositionEPD = Replace(listePositionEPD, reponse, "")
            Else
                reponse = reponse.Substring(reponse.IndexOf("|") + 1)
                reponse = epd & "|" & reponse.Substring(0, reponse.IndexOf("|") + 1)
                listePositionEPD = Replace(listePositionEPD, reponse, "")
            End If

            'on décharge le moteur
            dechargerMoteur()
            Do
                Threading.Thread.Sleep(1000)
            Loop While My.Computer.FileSystem.FileExists(Replace(fichierBIN, "experience.bin", "experience_new.bin"))

            'en travaux
            'on met à jour la profondeur dans experience.bin
            lectureBIN = New IO.FileStream(fichierBIN, IO.FileMode.Open, IO.FileAccess.ReadWrite, IO.FileShare.ReadWrite)
            posLecture = 0
            While posLecture < tailleBIN
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)

                'position
                If tabBIN(0) = critereBIN(0) Then
                    If tabBIN(1) = critereBIN(1) Then
                        If tabBIN(2) = critereBIN(2) Then
                            If tabBIN(3) = critereBIN(3) Then
                                If tabBIN(4) = critereBIN(4) Then
                                    If tabBIN(5) = critereBIN(5) Then
                                        If tabBIN(6) = critereBIN(6) Then
                                            If tabBIN(7) = critereBIN(7) Then
                                                'profondeur
                                                If tabBIN(8) = profActuelle Then
                                                    'coup
                                                    If (tabBIN(16) = critereBIN(16) And tabBIN(17) = critereBIN(17)) _
                                                    Or (tabBIN(16) = critereBINbis(16) And tabBIN(17) = critereBINbis(17)) Then
                                                        'pour effacer ce coup,
                                                        If posLecture >= 24 Then
                                                            'on se place sur l'entrée précédente
                                                            lectureBIN.Position = posLecture - 24
                                                            'on lit l'entrée précédente
                                                            lectureBIN.Read(tabBINtmp, 0, tabBINtmp.Length)

                                                            'on l'écrase avec les données de celui d'avant
                                                            For j = 0 To 23
                                                                lectureBIN.Position = posLecture + j
                                                                lectureBIN.WriteByte(tabBINtmp(j))
                                                            Next
                                                        Else
                                                            'on lit l'entrée suivante
                                                            lectureBIN.Read(tabBINtmp, 0, tabBINtmp.Length)
                                                            'on se place sur l'entrée actuelle
                                                            lectureBIN.Position = posLecture

                                                            'ou celui d'après
                                                            For j = 0 To 23
                                                                lectureBIN.Position = posLecture + j
                                                                lectureBIN.WriteByte(tabBINtmp(j))
                                                            Next
                                                        End If

                                                        'dans tous les cas on sort ! (Sinon si le fichier n'était pas défragmenté, ça peut boucler très très longtemps)
                                                        Exit While
                                                    End If
                                                End If
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    End If
                End If

                posLecture = lectureBIN.Position
                Application.DoEvents()
            End While
            tabBIN = Nothing
            lectureBIN.Close()


            'on recharge le moteur
            chargerMoteur(lblBrainLearn.Text)
            tailleBIN = 0
            If My.Computer.FileSystem.FileExists(fichierBIN) Then
                tailleBIN = FileLen(fichierBIN)
            End If

            'on met à jour l'affichage
            If indexEPD >= 0 And moteur_court <> "" Then
                If tabEPD(indexEPD) <> "" Then
                    afficherBIN(tabEPD(indexEPD))
                End If
            End If

        End If

        txtRecherche.Enabled = True
        Cursor = Cursors.Default
    End Sub

    Private Sub lblCoup1_Click(sender As Object, e As EventArgs) Handles lblCoup1.Click
        majRecherche(lblCoup1.Text)
    End Sub

    Private Sub lblCoup2_Click(sender As Object, e As EventArgs) Handles lblCoup2.Click
        majRecherche(lblCoup2.Text)
    End Sub

    Private Sub lblCoup3_Click(sender As Object, e As EventArgs) Handles lblCoup3.Click
        majRecherche(lblCoup3.Text)
    End Sub

    Private Sub lblCoup4_Click(sender As Object, e As EventArgs) Handles lblCoup4.Click
        majRecherche(lblCoup4.Text)
    End Sub

    Private Sub lblCoup5_Click(sender As Object, e As EventArgs) Handles lblCoup5.Click
        majRecherche(lblCoup5.Text)
    End Sub

    Private Sub lblCoup6_Click(sender As Object, e As EventArgs) Handles lblCoup6.Click
        majRecherche(lblCoup6.Text)
    End Sub

    Private Sub lblCoup7_Click(sender As Object, e As EventArgs) Handles lblCoup7.Click
        majRecherche(lblCoup7.Text)
    End Sub

    Private Sub lblCoup8_Click(sender As Object, e As EventArgs) Handles lblCoup8.Click
        majRecherche(lblCoup8.Text)
    End Sub

    Private Sub lblCoup9_Click(sender As Object, e As EventArgs) Handles lblCoup9.Click
        majRecherche(lblCoup9.Text)
    End Sub

    Private Sub lblCoup10_Click(sender As Object, e As EventArgs) Handles lblCoup10.Click
        majRecherche(lblCoup10.Text)
    End Sub

    Private Sub lblCoup11_Click(sender As Object, e As EventArgs) Handles lblCoup11.Click
        majRecherche(lblCoup11.Text)
    End Sub

    Private Sub lblCoup12_Click(sender As Object, e As EventArgs) Handles lblCoup12.Click
        majRecherche(lblCoup12.Text)
    End Sub

    Private Sub lblCoup13_Click(sender As Object, e As EventArgs) Handles lblCoup13.Click
        majRecherche(lblCoup13.Text)
    End Sub

    Private Sub lblCoup14_Click(sender As Object, e As EventArgs) Handles lblCoup14.Click
        majRecherche(lblCoup14.Text)
    End Sub

    Private Sub lblCoup15_Click(sender As Object, e As EventArgs) Handles lblCoup15.Click
        majRecherche(lblCoup15.Text)
    End Sub

    Private Sub lblCoup16_Click(sender As Object, e As EventArgs) Handles lblCoup16.Click
        majRecherche(lblCoup16.Text)
    End Sub

    Private Sub lblCoup17_Click(sender As Object, e As EventArgs) Handles lblCoup17.Click
        majRecherche(lblCoup17.Text)
    End Sub

    Private Sub lblCoup18_Click(sender As Object, e As EventArgs) Handles lblCoup18.Click
        majRecherche(lblCoup18.Text)
    End Sub

    Private Sub lblCoup19_Click(sender As Object, e As EventArgs) Handles lblCoup19.Click
        majRecherche(lblCoup19.Text)
    End Sub

    Private Sub lblCoup20_Click(sender As Object, e As EventArgs) Handles lblCoup20.Click
        majRecherche(lblCoup20.Text)
    End Sub

    Private Sub majRecherche(coupUCI As String)
        If Not cmdLignePrecedente.Visible And Not cmdLigneSuivante.Visible And coupUCI <> "" Then
            Cursor = Cursors.WaitCursor
            txtRecherche.Enabled = False
            ReDim tabEPD(0)
            indexEPD = 0
            If txtRecherche.Text <> "" And InStr(lblRecherche.Text, "EPD", CompareMethod.Text) > 0 Then
                If InStr(txtRecherche.Text, "moves", CompareMethod.Text) > 0 Then
                    txtRecherche.Text = Trim(txtRecherche.Text & " " & coupUCI)
                Else
                    txtRecherche.Text = Trim(txtRecherche.Text & " moves " & coupUCI)
                End If
                tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, txtRecherche.Text.Substring(txtRecherche.Text.IndexOf(" moves ") + 7), txtRecherche.Text.Substring(0, txtRecherche.Text.IndexOf(" moves ")))
            Else
                txtRecherche.Text = Trim(txtRecherche.Text & " " & coupUCI)
                lblRecherche.Text = "Suite UCI :"
                tabEPD(indexEPD) = moteurEPD(lblBrainLearn.Text, txtRecherche.Text)
            End If

            lblInfo.Text = tabEPD(indexEPD)
            pbEchiquier.Refresh()
            If indexEPD >= 0 And moteur_court <> "" Then
                If tabEPD(indexEPD) <> "" Then
                    afficherBIN(tabEPD(indexEPD))
                End If
            End If
            txtRecherche.Enabled = True
            Cursor = Cursors.Default
        End If
    End Sub

    Private Sub lblScore1_DoubleClick(sender As Object, e As EventArgs) Handles lblScore1.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup1.Text, lblScore1.Text)
    End Sub

    Private Sub lblScore2_DoubleClick(sender As Object, e As EventArgs) Handles lblScore2.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup2.Text, lblScore2.Text)
    End Sub

    Private Sub lblScore3_DoubleClick(sender As Object, e As EventArgs) Handles lblScore3.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup3.Text, lblScore3.Text)
    End Sub

    Private Sub lblScore4_DoubleClick(sender As Object, e As EventArgs) Handles lblScore4.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup4.Text, lblScore4.Text)
    End Sub

    Private Sub lblScore5_DoubleClick(sender As Object, e As EventArgs) Handles lblScore5.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup5.Text, lblScore5.Text)
    End Sub

    Private Sub lblScore6_DoubleClick(sender As Object, e As EventArgs) Handles lblScore6.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup6.Text, lblScore6.Text)
    End Sub

    Private Sub lblScore7_DoubleClick(sender As Object, e As EventArgs) Handles lblScore7.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup7.Text, lblScore7.Text)
    End Sub

    Private Sub lblScore8_DoubleClick(sender As Object, e As EventArgs) Handles lblScore8.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup8.Text, lblScore8.Text)
    End Sub

    Private Sub lblScore9_DoubleClick(sender As Object, e As EventArgs) Handles lblScore9.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup9.Text, lblScore9.Text)
    End Sub

    Private Sub lblScore10_DoubleClick(sender As Object, e As EventArgs) Handles lblScore10.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup10.Text, lblScore10.Text)
    End Sub

    Private Sub lblScore11_DoubleClick(sender As Object, e As EventArgs) Handles lblScore11.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup11.Text, lblScore11.Text)
    End Sub

    Private Sub lblScore12_DoubleClick(sender As Object, e As EventArgs) Handles lblScore12.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup12.Text, lblScore12.Text)
    End Sub

    Private Sub lblScore13_DoubleClick(sender As Object, e As EventArgs) Handles lblScore13.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup13.Text, lblScore13.Text)
    End Sub

    Private Sub lblScore14_DoubleClick(sender As Object, e As EventArgs) Handles lblScore14.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup14.Text, lblScore14.Text)
    End Sub

    Private Sub lblScore15_DoubleClick(sender As Object, e As EventArgs) Handles lblScore15.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup15.Text, lblScore15.Text)
    End Sub

    Private Sub lblScore16_DoubleClick(sender As Object, e As EventArgs) Handles lblScore16.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup16.Text, lblScore16.Text)
    End Sub

    Private Sub lblScore17_DoubleClick(sender As Object, e As EventArgs) Handles lblScore17.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup17.Text, lblScore17.Text)
    End Sub

    Private Sub lblScore18_DoubleClick(sender As Object, e As EventArgs) Handles lblScore18.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup18.Text, lblScore18.Text)
    End Sub

    Private Sub lblScore19_DoubleClick(sender As Object, e As EventArgs) Handles lblScore19.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup19.Text, lblScore19.Text)
    End Sub

    Private Sub lblScore20_DoubleClick(sender As Object, e As EventArgs) Handles lblScore20.DoubleClick
        majProfondeur(entree, sortie, tabEPD(indexEPD), lblCoup20.Text, lblScore20.Text)
    End Sub

    Private Sub majProfondeur(entreeScore As System.IO.StreamWriter, sortieScore As System.IO.StreamReader, epd As String, coup As String, scoreDepth As String)
        Dim critereBIN(23) As Byte, nouvelleProf As Integer, reponse As String, profActuelle As Integer
        Dim lectureBIN As IO.FileStream, tabBIN(23) As Byte, posLecture As Long

        If coup = "" Then
            Exit Sub
        End If
        Cursor = Cursors.WaitCursor
        txtRecherche.Enabled = False

        profActuelle = CInt(Replace(scoreDepth.Substring(scoreDepth.IndexOf("/") + 1), "}", ""))

        '    inversed key (8 octets) | Depth |          | score       | inv. move |       | Performance |
        'hex  0  1  2  3  4  5  6  7 |  8    |  9  A  B |  C  D  E  F |  0  1     |  2  3 |  4          |  5  6  7 (24 octets)
        'dec 00 01 02 03 04 05 06 07   08      09 10 11   12 13 14 15   16 17       18 19   20            21 22 23 tabBIN
        '-----------------------------------------------------------------------------------------------------
        'hex fb 59 2f 56 d4 01 8f 8f | 1e    | 00 00 00 | 3f 00 00 00 | 1c 03     | 00 00 | 64          | 00 00 00
        'dec                           30                 0000003f      031c                100%
        '                                                 63/208        001 100 011 100
        '                                                 +0.30         2   e   4   e
        '                                                               e2e4
        reponse = InputBox("New depth value ?", "min 4 <= depth value <= max 245", profActuelle)
        If reponse <> "" Then
            If IsNumeric(reponse) Then
                nouvelleProf = CInt(reponse)
                If 4 <= nouvelleProf And nouvelleProf <= 245 And profActuelle <> nouvelleProf Then
                    'on prépare un critère de recherche basé sur la position en cours et le coup sélectionné
                    Array.Copy(epdToEXP(entreeScore, sortieScore, epd), 0, critereBIN, 0, 8) '0-7
                    Array.Copy(moveToBIN(coup), 0, critereBIN, 16, 2) '0-1

                    'on efface cette position de la liste
                    reponse = listePositionEPD.Substring(listePositionEPD.IndexOf(epd))
                    If reponse = epd & "||" Then
                        listePositionEPD = Replace(listePositionEPD, reponse, "")
                    Else
                        reponse = reponse.Substring(reponse.IndexOf("|") + 1)
                        reponse = epd & "|" & reponse.Substring(0, reponse.IndexOf("|") + 1)
                        listePositionEPD = Replace(listePositionEPD, reponse, "")
                    End If

                    'on décharge le moteur
                    dechargerMoteur()
                    Do
                        Threading.Thread.Sleep(1000)
                    Loop While My.Computer.FileSystem.FileExists(Replace(fichierBIN, "experience.bin", "experience_new.bin"))

                    'on met à jour la profondeur dans experience.bin
                    lectureBIN = New IO.FileStream(fichierBIN, IO.FileMode.Open, IO.FileAccess.ReadWrite, IO.FileShare.ReadWrite)
                    posLecture = 0
                    While posLecture < tailleBIN
                        lectureBIN.Read(tabBIN, 0, tabBIN.Length)

                        'position
                        If tabBIN(0) = critereBIN(0) Then
                            If tabBIN(1) = critereBIN(1) Then
                                If tabBIN(2) = critereBIN(2) Then
                                    If tabBIN(3) = critereBIN(3) Then
                                        If tabBIN(4) = critereBIN(4) Then
                                            If tabBIN(5) = critereBIN(5) Then
                                                If tabBIN(6) = critereBIN(6) Then
                                                    If tabBIN(7) = critereBIN(7) Then
                                                        'coup
                                                        If tabBIN(16) = critereBIN(16) And tabBIN(17) = critereBIN(17) Then
                                                            'profondeur
                                                            If tabBIN(8) <> nouvelleProf Then
                                                                lectureBIN.Position = posLecture + 8
                                                                lectureBIN.WriteByte(nouvelleProf)
                                                                'dans tous les cas on sort ! (Sinon si le fichier n'était pas défragmenté, ça peut boucler très très longtemps)
                                                                Exit While
                                                            End If
                                                        End If
                                                    End If
                                                End If
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If

                        posLecture = lectureBIN.Position
                        Application.DoEvents()
                    End While
                    tabBIN = Nothing
                    lectureBIN.Close()

                    'on recharge le moteur
                    chargerMoteur(lblBrainLearn.Text)
                    tailleBIN = 0
                    If My.Computer.FileSystem.FileExists(fichierBIN) Then
                        tailleBIN = FileLen(fichierBIN)
                    End If

                    'on met à jour l'affichage
                    If indexEPD >= 0 And moteur_court <> "" Then
                        If tabEPD(indexEPD) <> "" Then
                            afficherBIN(tabEPD(indexEPD))
                        End If
                    End If

                End If
            End If
        End If

        txtRecherche.Enabled = True
        Cursor = Cursors.Default
    End Sub

    Private Sub lblInfo_MouseClick(sender As Object, e As MouseEventArgs) Handles lblInfo.MouseClick
        If e.Button = Windows.Forms.MouseButtons.Right Then
            If lblRecherche.Text = "Position EPD :" Then
                If MsgBox("Copy " & tabEPD(indexEPD) & " into clipboard ?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                    My.Computer.Clipboard.SetText(tabEPD(indexEPD))
                End If
            ElseIf lblRecherche.Text = "Suite UCI :" Then
                If MsgBox("Copy " & tabEPD(indexEPD) & " into clipboard ?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                    My.Computer.Clipboard.SetText(tabEPD(indexEPD))
                ElseIf MsgBox("Copy " & txtRecherche.Text & " into clipboard ?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                    My.Computer.Clipboard.SetText(txtRecherche.Text)
                End If
            ElseIf lblRecherche.Text = "Mode PGN :" Then
                If MsgBox("Copy " & lblInfo.Text.Substring(lblInfo.Text.IndexOf(":") + 2) & " into clipboard ?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                    My.Computer.Clipboard.SetText(lblInfo.Text.Substring(lblInfo.Text.IndexOf(":") + 2))
                End If
            End If
        End If

    End Sub

    
End Class
