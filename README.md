# Techtobo portal

A Shiny app that is both the public Techtobo website and a private portal.

- **Public site:** home, services, about, our clients, contact, and a working "Request a quotation" form.
- **Administrators** (the directors): dashboard, quotation requests from the website, job register, create quotations and invoices (branded PDF), record eTIMS CU invoice numbers, record payments and print receipts, manage clients, settings and user accounts. Administrators log in from "Log in" or from the "Administrator login" link in the footer, which accepts administrator accounts only.
- **Clients (e.g. Jalisa Limited):** read-only view of their own jobs, quotations and invoices, with PDF download, plus their own quotation requests. Clients do not see your costs, other clients, or draft quotations. An administrator creates these logins.
- **Users:** anyone can sign up from the website with their email and a password. A user can request quotations and see the status of their own requests, and nothing else. An administrator can turn a user into a client login under Settings > Portal users.

Passwords are stored only as salted hashes. There is no default administrator account or password: the first administrator is created once, using the setup key.

### Quotation requests

Requests are saved in the `requests` tab of the data store with a reference (REQ-2026-001), the date and time, and the requester's details. Administrators see them under **Quotation requests**, where they set a status (New, Contacted, Quoted, Closed) and keep internal notes. The portal does not send an email when a request arrives; check the dashboard, which shows the number of new requests.

### Adding a client to "Our Clients"

Edit `PUBLIC_CLIENTS` in `R/config.R` and add a line. To show a logo, save it in `www/clients/` under the file name given there (for Jalisa: `www/clients/jalisa-limited.png`). Without a logo file, the client's name is shown as text. After adding files, run `rsconnect::writeManifest()` again before pushing.

## 1. Try it on your laptop (10 minutes)

1. Open this folder in RStudio and open `app.R`.
2. Install the packages once:
   ```r
   install.packages(c("shiny", "bslib", "DT", "googlesheets4", "sodium", "png"))
   ```
3. Click **Run App**.
4. Click **Portal login**. The first time, it asks you to create the first administrator account.

With no further setup the app stores records as CSV files in a `data/` folder next to `app.R`. That is fine for testing. For the hosted version, use a Google Sheet (next section), because free hosting does not keep local files.

## 2. Set up the Google Sheet (one time)

1. Create a new, empty Google Sheet in the company Google account. Name it e.g. "Techtobo Portal Data".
2. Copy the sheet ID from its address: `https://docs.google.com/spreadsheets/d/`**`THIS_PART`**`/edit`.
3. Go to console.cloud.google.com, create a project, and enable the **Google Sheets API**.
4. Under **IAM & Admin > Service Accounts**, create a service account. Open it, go to **Keys > Add key > JSON**, and download the key file. Keep this file private. Do not put it in this folder or on GitHub.
5. Share the Google Sheet with the service account's email address (it ends in `iam.gserviceaccount.com`) as **Editor**.

The app creates the tabs it needs (settings, users, clients, jobs, documents, lines, receipts) the first time it runs. Directors can open the sheet directly to view or back up the data, but should not rename tabs or column headings.

To test the Google Sheet from your laptop, create a file named `.Renviron` in this folder with:

```
TECHTOBO_SHEET_ID=your-sheet-id
GOOGLE_APPLICATION_CREDENTIALS=/full/path/to/the/key.json
```

Restart R and run the app again.

## 3. Publish on Posit Connect Cloud

1. In RStudio, with this folder as the working directory, create the manifest file that tells Connect Cloud which packages to install:
   ```r
   install.packages("rsconnect")
   rsconnect::writeManifest()
   ```
2. Put the folder in a GitHub repository. The `.gitignore` file already excludes `data/`, `.Renviron` and key files. The code itself contains no passwords or keys.
3. Sign in at connect.posit.cloud with GitHub, choose **Publish > Shiny**, pick the repository and `app.R`.
4. Before publishing, open **Advanced settings** and add these secret variables:

   | Name | Value |
   |---|---|
   | `TECHTOBO_SHEET_ID` | The sheet ID from step 2 |
   | `GS_SERVICE_ACCOUNT_JSON` | The whole contents of the JSON key file, pasted in |
   | `TECHTOBO_SETUP_KEY` | A long phrase you choose. It is needed once, to create the first administrator account |

5. Publish. Open the site, click **Portal login**, and create the first administrator account using the setup key. Then add the other administrator and the Jalisa login under **Settings > Portal users**.

## 4. Daily use

1. **Jobs:** add a job when Jalisa reports a requirement. Update its status as it progresses.
2. **New quotation / invoice:** choose the client and job, add line items (each tagged Labour, Materials, Equipment or Other), and save. Numbers are assigned automatically (QTN-2026-001, INV-2026-001). For an invoice you can copy the lines from an earlier quotation.
3. **Documents:** select a row and download the branded PDF.
4. **eTIMS:** issue the tax invoice in KRA eTIMS with the same lines and totals, then select the invoice in **Documents**, type the CU invoice number and save. It then prints on the PDF. The portal PDF is a branded invoice; the eTIMS invoice is the tax invoice.
5. **Receipts:** when Jalisa pays, record the payment against the invoice. The invoice and job are marked Paid when the balance reaches zero, and you can download a receipt PDF.
6. **Settings:** fill in the KRA PIN, bank and M-Pesa details once. Set **VAT-registered** to Yes only if the company has a VAT obligation on iTax; new documents then add VAT at the rate shown.

## 5. What it does not do yet

- **Direct eTIMS submission.** KRA only allows this through certified OSCU or VSCU integration. The portal is built so this can be added later without changing how you work.
- **Editing a saved quotation or invoice.** Saved documents are fixed, as financial records should be. To correct one, set its status to Cancelled (or Declined) and create a new one. If the eTIMS invoice was already issued, raise a credit note in eTIMS as well.
- **Job completion reports and work orders** are still filled in from the Word template.
- **Password reset by email.** A director resets a password under Settings by re-entering the username with a new password.

## 6. Files

| File | Purpose |
|---|---|
| `app.R` | The website, portal screens and logic |
| `R/config.R` | Company details, service list, statuses, data columns |
| `R/storage.R` | Reads and writes records (Google Sheet or local CSV) |
| `R/pdf.R` | Draws the quotation, invoice and receipt PDFs |
| `www/` | Logos and styling |

Company details printed on documents (address, phone, email, company number) are in `R/config.R`.
