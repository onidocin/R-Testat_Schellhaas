# R-Testat_Schellhaas
Autor: Nico Schellhaas

Dies ist mein R-Testat für den DxI-Unterricht vom Studiengang ADLS der ZHAW im ersten Semester.
Die Aufgabestellung lautet wie folgt:
Sie sollen die Wohnbevölkerung nach Kanton und nach Bezirk für die Jahre 2022 und 2010 untersuchen.

    1. Kombinieren Sie die Tabellen der beiden Jahre, so dass die Jahreszahl für jeden Datensatz ebenfalls ersichtlich ist. 
    2. Erzeugen Sie je einen Sekundärindex für die Kantone und einen für die Bezirke, so dass für jeden
    Gemeindendatensatz auch der Kantons- und Bezirksname ersichtlich ist.
    3. Organisieren Sie die Wohnbevölkerung nach den folgenden Altersgruppen: 
        Kinder (Bis 12 Jahre)
        Minderjährige (unter 18 Jährige)
        Erwachsene, jünger als 65 Jahre
        Erwachsene, ab 65 Jahren
    Beachten Sie, dass ab Lebensjahr 100 alle Einwohnenden zusammengefasst wurden und dieser Spaltenname keine Zahl ist. Nehmen Sie hierfür eine geeignete Kodierung vor.
    4. Bestimmen Sie die statistischen Kennwerte für die drei Altersgruppen über alle Datensätze der Jahre 2010 und 2022 der Originalquelle und speichern Sie Ihre Ergebnisse als Tabelle.
    5. Ermitteln Sie den Unterschied des Durchschnittsalters dieser Gruppen in den beiden Referenzjahren für alle Bezirke im Kanton Zürich und speichern Sie diese Unterschiede als separate Tabelle.
    6. Erstellen Sie einen Boxplot, aus dem die Verteilungen des Durchschnittsalters der Minderjährigen in den Gemeinden der 11 Bezirke des Kanton Zürichs ohne die Stadt Zürich in den beiden Jahren hervorgehen.

Für die Bearbeitung der aufgeführten Punkte verwenden Sie R. Fassen Sie ähnliche Arbeitschritte zusammen und vermeiden Sie redundanten Code. Alle Arbeitsschritte lassen sich am leichtesten mit den tidyverse-Bibliotheken lösen.

# Organisation
-   Archiv: Beinhaltet meinen ersten Versuch des R-Skripts und dient nur der genaueren Dokumentierung
-   Resultate: In diesem Ordner befinden sich der erzeugte Boxplot und die ebenfalls erzeugten beiden Tabellen.
-   Rohdaten: Hier abgelegt ist die gegebene Excel-Tabelle, welche ich für die erstellung des Skripts benutzt habe.
-   Skript: Die fertige R-Datei welches als Produkt dieses Testat fungiert.