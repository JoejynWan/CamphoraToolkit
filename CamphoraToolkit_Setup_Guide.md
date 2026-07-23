# Camphora Toolkit — Setup Guide

**Who this is for:** anyone who needs to use the Camphora Toolkit but has never
written a line of code. You do not need to understand any code to follow this.

**What you are doing:** installing two free programs (R and RStudio), then
running one file that opens the Toolkit in your web browser.

**How long it takes:** about 15 minutes of clicking, plus a long wait the first
time you run the Toolkit (see Step 6). After the first time, opening the Toolkit
takes about a minute.

---

## Step 1 — Install R

R is the engine that runs the Toolkit. You will never need to open R directly.

1. Go to **https://cran.r-project.org/**
2. Click **Download R for Windows** (or **Download R for macOS** on a Mac).
3. Click **base**, then click the big download link at the top
   (it will say something like *Download R-4.x.x for Windows*).
4. Open the file you just downloaded and click through the installer.
   **Accept all the default options** — do not change anything.

> On a Mac, choose the `.pkg` file that matches your machine. If you're unsure
> whether your Mac is Apple Silicon or Intel, click the Apple menu →
> *About This Mac*. "Apple M1/M2/M3/M4" means Apple Silicon (`arm64`).

---

## Step 2 — Install RStudio

RStudio is the window you'll actually look at. It's just a friendlier way to
use R.

1. Go to **https://posit.co/download/rstudio-desktop/**
2. Scroll to the download section. It shows two steps — you already did step 1
   (Install R), so click the button under **2: Install RStudio**.
3. Open the downloaded file and click through the installer, again
   **accepting all the defaults**.

**Important:** install R *before* RStudio. If you did it the other way round,
just install R and then restart RStudio — it will find R automatically.

---

## Step 3 — Save the launcher file

The launcher file **`CamphoraToolkit_Launcher.R`** is saved in Google Drive: 
Shared drives\11_Office_Database\R Scripts\CamphoraToolkit\CamphoraToolkit_Launcher.R

1. Save it somewhere you can easily find again — e.g., your **Documents** folder. 
2. Do **not** rename it, and make sure the name still ends in `.R`.

> If Windows asks what program to open `.R` files with, you can ignore that for
> now — Step 4 opens it from inside RStudio instead.

---

## Step 4 — Open the launcher in RStudio

1. Open **RStudio** from your Start menu (not R — RStudio).
2. In the top menu, click **File → Open File…**
3. Find and select `CamphoraToolkit_Launcher.R`, then click **Open**.

The file's contents will appear in a panel, usually the top-left. It will look
like a wall of code. **You do not need to read or understand any of it.**

---

## Step 5 — Run the launcher

Look at the **top-right corner of the panel showing the code**. There is a
button labelled **Source** (it has a small arrow icon).

**Click `Source` once.**

That's the only action you need to take. (Keyboard shortcut, if you prefer:
`Ctrl` + `Shift` + `S` on Windows, `Cmd` + `Shift` + `S` on Mac.)

---

## Step 6 — Wait (the first time only)

The bottom-left panel — the **Console** — will start scrolling text. This is
normal. It is downloading and installing everything the Toolkit needs.

⏳ **The first time, this can take 20–40 minutes.** It is genuinely doing work,
not frozen. Leave it running and go make a coffee.

Two things you might see while waiting:

- **A question in the Console asking something like**
  *"Do you want to install from sources the package which needs compilation?"*
  → Type **`n`** and press **Enter**. (Do this for every time it asks.)

- **A pop-up asking to allow access through the firewall**
  → Click **Allow**.

**Every time after this first run, it only takes about a minute** — the packages
are already installed and it just fetches the latest version of the Toolkit.

---

## Step 7 — The Toolkit opens

When it's finished, the Toolkit will open automatically.

**Keep RStudio open in the background while you use the Toolkit.** RStudio is
what's running it — if you close RStudio, the Toolkit stops working.

When you're finished, you may just close the Toolkit and RStudio. 

---

## Every time after this

You do **not** need to reinstall anything. To open the Toolkit again:

1. Open RStudio.
2. `File → Open File…` → `CamphoraToolkit_Launcher.R`
   *(or use `File → Recent Files`, where it will now be listed)*
3. Click **Source**.
4. Wait about a minute.

The launcher automatically downloads the newest version of the Toolkit each
time, so **you never need to download an updated Toolkit yourself.**

---

## If something goes wrong

| What you see | What to do |
|---|---|
| Console asks *"install from sources…?"* | Type `n`, press Enter |
| Toolkit closed by itself | Check RStudio is still open — closing it stops the Toolkit |
| Red text mentioning `curl`, `download`, or `cannot open URL` | Check your internet connection. If you're on a work network or VPN, the firewall may be blocking GitHub — contact Joejyn |
| Red text naming a package that failed to install | Note the package name and send it to Joejyn |
| Nothing seems to happen after clicking Source | Look at the **Console** (bottom-left) — the last line usually explains why |

**When asking for help, the most useful thing you can send is a screenshot of
the whole RStudio window**, including the Console panel at the bottom. The last
few lines of red text are what matter.

---

## Optional — extra tools for certain apps

Most of the Toolkit works with just R and RStudio. A few specific tools need
extra software installed separately, because these are *not* R packages and the
launcher cannot install them for you:

| Tool in the Toolkit | Extra software needed |
|---|---|
| Camera trap processing | **ExifTool** — https://exiftool.org |
| Arbo Report (Word output) | **Pandoc** — usually already included with RStudio |
| Anything producing images | **ImageMagick** — https://imagemagick.org |

Only install these if you are told you need them, or if an app gives an error
mentioning one of them by name.

---

## What's next

Once the Toolkit is open, you'll see the list of available tools. Some of them
read project folders directly from Google Drive — if that applies to the tool
you need, make sure **Google Drive for Desktop** is installed and signed in
before using it.

---

*Questions or problems: contact Joejyn.*
