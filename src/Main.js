const fs = require('fs')
const cwd = process.cwd()

const Elm = require([cwd, 'dist/app.js'].join('/'))
const app = Elm.Main.worker()

const READ_DOCS = 'ReadDocs'
const CREATE_DOCS_FILES = 'CreateDocsFiles'

function readDocs(docsFile) {
  return {
    msg: READ_DOCS,
    docsFile: docsFile
  }
}

// Equivalent to subscriptions in Elm.
app.ports.toJs.subscribe(function(msg) {
  update(msg)
})

// Equivalent to update in Elm.
function update(msg) {
  switch(msg.msg) {
    case CREATE_DOCS_FILES:
      createDirectory('docs')
      writeDocsFiles(msg.docsFiles)
      break
    case READ_DOCS:
      sendJsonDocs(msg.docsFile)
      break
  }
}

function createDirectory(directoryName) {
  if (!fs.existsSync('docs')) {
    fs.mkdirSync('docs')
  }
}

function writeDocsFiles(docsFiles) {
  docsFiles.forEach(function(content) {
    Object.keys(content).forEach(function(key) {
      fs.writeFileSync('docs/' + key + '.html', content[key])
    })
  })
}

function sendJsonDocs(docsFile) {
  const fileName = [cwd, docsFile].join('/')
  const file = fs.readFileSync(fileName)
  const jsonDocs = JSON.parse(file.toString())
  app.ports.fromJs.send(jsonDocs)
}

// Main!
update(readDocs('docs.json'))
